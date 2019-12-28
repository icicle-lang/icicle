{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
-- | Parser for the Icicle Source language from the Sorbet lexer.
--
--   There were two ways of going about this, writing an elaborator
--   from the Sorbet Concrete parse tree, or writing a new AST parser.
--
--   Having a look at the Sorbet Concrete Parser, it looked to me
--   like a few very important things were missing, and the Concrete
--   syntax of Sorbet is quite different to the original AST of
--   Icicle.
--
--   Also, Sorbet is quite a bit richer, and I can't actually support
--   all of it as written at the moment.
--
--   I decided to go down this route, as I can share as much as I
--   need from both of the current sources, and can incrementally
--   update the Icicle AST and new parser to get closer to Sorbet
--   over time.
module Icicle.Sorbet.Abstract.Parser (
    Parser
  , Decl (..)

  , pTop
  , pQuery
  , pDecls
  ) where

import qualified Data.List as List
import           Data.Scientific (toRealFloat)

import           Icicle.Sorbet.Abstract.Tokens
import           Icicle.Sorbet.Abstract.Type

import           Icicle.Sorbet.Lexical.Syntax
import           Icicle.Sorbet.Position

import           Icicle.Common.Base as Common
import           Icicle.Data.Name
import           Icicle.Data.Time (Date (..), midnight)
import           Icicle.Source.Query
import           Icicle.Source.Type
import           Icicle.Source.Parser.Constructor (checkPat, constructors)
import           Icicle.Source.Parser.Operators
import           Icicle.Source.Checker.Function (Decl (..))

import           P

import           Text.Megaparsec (choice)
import qualified Text.Megaparsec as Mega

type Var = Variable


pTop :: Parser s m => OutputId -> m (QueryTop Position Var)
pTop name = do
  _ <- pToken Tok_From                                    <?> "feature start"
  v <- pUnresolvedInputId                                 <?> "input source"
  _ <- pContextEnd
  q <- pQuery                                             <?> "query"
  return $ QueryTop v name q


pQuery :: Parser s m => m (Query Position Var)
pQuery = do
  cs <- pContexts                                         <?> "contexts"
  x  <- pExp                                              <?> "expression"
  return $ Query cs x



pDecls :: Parser s m => m (Position, [Decl Position Var])
pDecls =
  (,)
    <$> pToken Tok_LBrace
    <*> (pDecl `Mega.sepEndBy` pToken Tok_Semi)
    <*  pToken Tok_RBrace


pDecl :: Parser s m => m (Decl Position Var)
pDecl = do
  -- Read a variable first, this is the function name
  -- in either its definition or type.
  (pos, var) <- pVariable

  -- Read the rest of the function or type signature.
  DeclFun pos var <$> pFunction <|> DeclType pos var <$> pDeclType


pDeclType :: Parser s m => m (Scheme Var)
pDeclType = do
  _ <- pToken Tok_Colon
  pConstrainedType



pFunction :: Parser s m => m (Exp Position Var)
pFunction = do
  vs  <- many pVariable                     <?> "function variables"
  _   <- pToken Tok_Equals                  <?> "equals"
  q   <- pQuery
  pure $ foldr (uncurry Lam) (simpNested q) vs


-- | Parse a potentially empty list of contexts.
pContexts :: Parser s m => m [Context Position Var]
pContexts = do
  pSomeContexts <|> pure []


-- | Parse contexts, all of which are followed by `Tok_In`.
--
--   This is a little more complex than it aught to be, as
--   in the AST, let bindings are a single item, where as
--   in the Concrete syntax, we can take multiple. So we have
--   here a more custom "many" and "some" to operate on these.
pSomeContexts :: Parser s m => m [Context Position Var]
pSomeContexts = do
  cs   <- pContextLet <|> some pSingleContext
  _    <- pContextEnd
  rest <- pContexts
  pure  $ cs <> rest


-- | End of a context.
--
--   Both `in` and `~>` are available. For programs `in` is
--   nicer, but on the repl, `~>` seems to divide things more
--   nicely.
pContextEnd :: Parser s m => m Position
pContextEnd = pToken Tok_In <|> pToken Tok_FlowsInto

-- | Parse a let context.
--
--   This should change soon to just ask for the Let, then call
--   pDecls.
--
--   The AST needs to change a bit though for that to happen.
pContextLet :: Parser s m => m [Context Position Var]
pContextLet = do
  _ <- pToken Tok_Let
  _ <- pToken Tok_LBrace

  let
    letFun = do
      (p, n) <- pVariable
      x      <- pFunction
      return  $ Let p (PatVariable n) x

    letPat = do
      pat    <- pPattern
      p      <- pToken Tok_Equals
      x      <- pExp
      return  $ Let p pat x

    letE   = letFun <|> letPat

  ret <- letE  `Mega.sepEndBy1` pToken Tok_Semi
  _   <- pToken Tok_RBrace
  return ret


-- Just singular contexts here, not lets, which
-- expand into multiple.
-- This will be the whole thing when lets are changed
-- to take a list of Decls.
pSingleContext :: Parser s m => m (Context Position Var)
pSingleContext =
  choice [
      pContextWindowed
    , pContextGroup
    , pContextDistinct
    , pContextFilter
    , pContextLatest
    , pContextFold
    ]


pContextGroup :: Parser s m => m (Context Position Var)
pContextGroup = do
  pos <- pToken Tok_Group
  pContextGroupBy pos <|> pContextGroupFold pos


pContextGroupBy :: Parser s m => Position -> m (Context Position Var)
pContextGroupBy pos =
  GroupBy pos
    <$> pExp


pContextGroupFold :: Parser s m => Position -> m (Context Position Var)
pContextGroupFold pos = do
  _      <- pToken Tok_Fold
  (k, v) <- keyval
  _      <- pToken Tok_Equals
  e      <- pExp
  pure $ GroupFold pos k v e
    where
  keyval
    = do p <- pPattern
         case p of
           PatCon ConTuple [k, v]
             -> return (k, v)
           _ -> mzero


pContextDistinct :: Parser s m => m (Context Position Var)
pContextDistinct =
  Distinct
    <$> pToken Tok_Distinct
    <*> pExp


pContextFilter :: Parser s m => m (Context Position Var)
pContextFilter =
  Filter
    <$> pToken Tok_Filter
    <*> pExp


pContextLatest :: Parser s m => m (Context Position Var)
pContextLatest =
  Latest
    <$> pToken Tok_Latest
    <*> fmap (fromInteger . snd) pInteger


pContextWindowed :: Parser s m => m (Context Position Var)
pContextWindowed = do
  pos <- pToken Tok_Windowed
  pContextBetween pos <|> pContextAfter pos


pContextAfter :: Parser s m => Position -> m (Context Position Var)
pContextAfter pos =
  Windowed pos
    <$> pWindowSizeUnit
    <*> pure Nothing


pContextBetween :: Parser s m => Position -> m (Context Position Var)
pContextBetween pos = do
  _  <- pToken Tok_Between
  t1 <- pWindowSizeUnit
  _  <- pToken Tok_And
  t2 <- pWindowSizeUnit
  return $ Windowed pos t2 $ Just t1



-- | Parse a Fold context.
--
--   The Sorbet CST as originally imagined cast folds more
--   as a function than a context; taking a starting value
--   and a lambda function for the cons.
--
--   For now, I've implemented these more like the original
--   AST.
pContextFold :: Parser s m => m (Context Position Var)
pContextFold = do
  (p, ft) <- pFoldType
  n       <- pPattern
  _       <- pToken Tok_Equals
  z       <- pQuery                                         <?> "initial value"
  _       <- pToken Tok_Then                                <?> "then"
  k       <- pQuery                                         <?> "fold expression"
  return $ LetFold p (Fold n (simpNested z) (simpNested k) ft)


pFoldType :: Parser s m => m (Position, FoldType)
pFoldType
    =   (pToken Tok_Fold1 `with` (, FoldTypeFoldl1))
    <|> (pToken Tok_Fold  `with` (, FoldTypeFoldl))


pPattern :: Parser s m => m (Pattern Var)
pPattern
 = do e <- pExp
      checkPat e


pExp :: Parser s m => m (Exp Position Var)
pExp = do
  xs <- some ((Left <$> pExp1) <|> pOp) <?> "expression"
  either (fail . renderDefixError) return (defix xs)

 where
  pOp = do (p, o) <- pVarOp <|> (, Operator ",") <$> pToken Tok_Comma
           return (Right (o,p))

pExp1 :: Parser s m => m (Exp Position Var)
pExp1
 =   (uncurry Var        <$> var        )
 <|> (uncurry Prim       <$> primitives )
 <|> (simpNested         <$> inParens)
 <|> parseIf
 <|> parseCase
 <?> "expression"
 where
  var
   = pVariable

  inParens
   = pToken Tok_LParen *> pQuery <* pToken Tok_RParen  <?> "sub-expression or nested query"

  parseIf
   = do pos   <- pToken Tok_If
        scrut <- pExp
        _     <- pToken Tok_Then
        true  <- pQuery
        _     <- pToken Tok_Else
        false <- pQuery
        return $ If pos scrut (simpNested true) (simpNested false)

  parseCase
   = do pos   <- pToken Tok_Case
        scrut <- pExp
        _     <- pToken Tok_Of
        _     <- pToken Tok_LBrace
        alts  <- parseAlt `Mega.sepEndBy` pToken Tok_Semi
        _     <- pToken Tok_RBrace
        return $ Case pos scrut alts

  parseAlt
   = do pat <- pPattern
        _   <- pToken Tok_Then
        xx  <- pQuery
        return (pat, simpNested xx)


pUnresolvedInputId :: Parser s m => m UnresolvedInputId
pUnresolvedInputId
 = tryToken get <?> "input identifier"
 where
  get _ (Tok_VarId v)  = parseUnresolvedInputId v
  get _ (Tok_ConId v)  = parseUnresolvedInputId v
  get _ (Tok_String v) = parseUnresolvedInputId v
  get _ _              = Nothing


primitives :: Parser s m => m (Position, Prim)
primitives
 =   (second (Lit . LitInt . fromInteger)      <$> pInteger)
 <|> (second (Lit . LitDouble . toRealFloat)   <$> pRational)
 <|> (second (Lit . LitString)                 <$> pString)
 <|> (second (Lit . LitTime . midnight . Date) <$> pDate)
 <|> second PrimCon                            <$> pConstructor
 <|> (flip (,) <$> timePrimitives <*> position)
 <?> "primitive"


pConstructor :: Parser s m => m (Position, Constructor)
pConstructor
 = do (p, Construct n) <- pConId
      case List.lookup n constructors of
        Just c -> return (p, c)
        Nothing -> fail ("Not a known constructor: " <> show n)



timePrimitives :: Parser s m => m Prim
timePrimitives
 =   Mega.try  (Fun (BuiltinTime DaysBetween)
            <$ pToken Tok_Days
            <* pToken Tok_Between)
 <|> Mega.try  (Fun (BuiltinTime DaysJulianEpoch)
             <$ pToken Tok_Days
             <* Mega.notFollowedBy (pToken Tok_Before <|> pToken Tok_After))
 <|> Mega.try  (Fun (BuiltinTime SecondsBetween)
            <$ pToken Tok_Seconds
            <* pToken Tok_Between)
 <|> Mega.try  (Fun (BuiltinTime SecondsJulianEpoch)
             <$ pToken Tok_Seconds
             <* Mega.notFollowedBy (pToken Tok_Before <|> pToken Tok_After))


pWindowSizeUnit :: Parser s m => m Common.WindowUnit
pWindowSizeUnit
 = do (_, i) <- second fromInteger <$> pInteger <?> "window amount"
      unit Tok_Days (Common.Days i)
        <|> unit Tok_Months (Common.Months i)
        <|> unit Tok_Weeks (Common.Weeks i)

 where
  unit kw q
   = pToken kw *> return q


simpNested :: Query a n -> Exp a n
simpNested q = case q of
  Query [] e
    -> e
  contextual
    -> Nested (annotOfQuery contextual) contextual
