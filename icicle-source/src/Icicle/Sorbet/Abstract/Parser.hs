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

  , pTop
  , pQuery
  , pModule
  , pUnresolvedInputId
  ) where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Foldable as Foldable
import qualified Data.Text as Text
import           Data.Scientific (toRealFloat)

import           Icicle.Sorbet.Abstract.Tokens
import           Icicle.Sorbet.Abstract.Type
import qualified Icicle.Sorbet.Abstract.Regex as Regex

import           Icicle.Sorbet.Lexical.Syntax
import           Icicle.Sorbet.Position

import           Icicle.Common.Base as Common
import           Icicle.Data.Name
import           Icicle.Data.Time (Date (..), midnight)
import           Icicle.Internal.Pretty (pretty)
import           Icicle.Source.Query
import           Icicle.Source.Type (valTypeOfType)
import           Icicle.Source.Parser.Constructor (checkPat, constructors)
import           Icicle.Source.Parser.Operators

import           P

import           Text.Megaparsec (choice)
import qualified Text.Megaparsec as Mega

type Var = Variable


pModule :: Parser s m => m (Module Range Var)
pModule = do
  name    <- pModuleName <|> pure (ModuleName "Default")


  let
    namespaceText = do
      (c,cs) <- Text.uncons (getModuleName name)
      parseNamespace $ Text.cons (Char.toLower c) cs

  nspace  <- maybe (fail "Invalid namespace") pure namespaceText

  _       <- pToken Tok_LBrace
  imports <- pImport `Mega.sepEndBy` pToken Tok_Semi
  decls   <- pModuleDecl nspace `Mega.sepEndBy` pToken Tok_Semi
  _       <- pToken Tok_RBrace
  return $
    Module name imports decls


pImport :: Parser s m => m (ModuleImport Range)
pImport = do
  _      <- pToken Tok_Import
  (p, s) <- pConstructorText
  return (ModuleImport p (ModuleName s))


pModuleName :: Parser s m => m ModuleName
pModuleName = do
  _ <- pToken Tok_Module
  n <- pConstructorText
  _ <- pToken Tok_Where
  return (ModuleName (snd n))


pConstructorText :: Parser s m => m (Range, Text)
pConstructorText = do
  (p, Construct x) <- pConId
  return (p, x)


pTop :: Parser s m => OutputId -> m (QueryTop Range Var)
pTop name = do
  _ <- pToken Tok_From                                    <?> "feature start"
  v <- pUnresolvedInputId                                 <?> "input source"
  _ <- pContextEnd
  q <- pQuery                                             <?> "query"
  return $ QueryTop v name q


pQuery :: Parser s m => m (Query Range Var)
pQuery = do
  cs <- pContexts                                         <?> "contexts"
  x  <- pExp                                              <?> "expression"
  return $ Query cs x


pModuleDecl :: Parser s m => Namespace -> m (Decl Range Var)
pModuleDecl nspace = pOutput nspace <|> pInput nspace <|> pDecl


pDecl :: Parser s m => m (Decl Range Var)
pDecl = do
  -- Read a variable first, this is the function name
  -- in either its definition or type.
  (pos, var) <- pVariable

  -- Read the rest of the function or type signature.
  DeclFun pos var Nothing <$> pFunction <|> pDeclTyped pos var


pDeclTyped :: Parser s m => Range -> Name Var -> m (Decl Range Var)
pDeclTyped pos typName = do
  _        <- pToken Tok_Colon
  s        <- pTypeScheme
  _        <- pToken Tok_Semi
  offset   <- Mega.getOffset
  (_, var) <- pVariable
  unless (typName == var) $
    failAtOffset offset $ List.unlines
      [ "Name declared in type annotation:"
      , "  " <> show (pretty typName)
      , ""
      , "doesn't match the name defined for the expression:"
      , "  " <> show (pretty var)
      ]

  DeclFun pos var (Just s) <$> pFunction


pFunction :: Parser s m => m (Exp Range Var)
pFunction = do
  vs  <- many pVariable                     <?> "function variables"
  _   <- pToken Tok_Equals                  <?> "equals"
  q   <- pQuery
  pure $ foldr (uncurry Lam) (simpNested q) vs



pInputName :: Parser s m => m InputName
pInputName = do
  (_, Variable x) <- pVarId
  maybe (fail "Couldn't parse as InputName") pure (parseInputName x)



pInput :: Parser s m => Namespace -> m (Decl Range n)
pInput ns = do
  a      <- pToken Tok_Input
  n      <- pInputName
  _      <- pToken Tok_Colon
  vt     <- pPositionedFail pType (valTypeOfType . snd) $ List.unlines [
              "Input streams must be serialisable types."
            , empty
            , "Function types, modalities, and type variables are not supported."
            ]

  pure $
    DeclInput a (InputId ns n) vt unkeyed


pOutputName :: Parser s m => m OutputName
pOutputName = do
  (_, Variable x) <- pVarId
  maybe (fail "Couldn't parse as OutputName") pure (parseOutputName x)


pOutput :: Parser s m => Namespace -> m (Decl Range Var)
pOutput ns = do
  a      <- pToken Tok_Feature
  n      <- pOutputName
  _      <- pToken Tok_Equals
  t      <- pTop (OutputId ns n)

  pure $
    DeclOutput a (OutputId ns n) t



-- | Parse a potentially empty list of contexts.
pContexts :: Parser s m => m [Context Range Var]
pContexts = do
  pSomeContexts <|> pure []


-- | Parse contexts, all of which are followed by `Tok_In`.
--
--   This is a little more complex than it aught to be, as
--   in the AST, let bindings are a single item, where as
--   in the Concrete syntax, we can take multiple. So we have
--   here a more custom "many" and "some" to operate on these.
pSomeContexts :: Parser s m => m [Context Range Var]
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
pContextEnd :: Parser s m => m Range
pContextEnd = pToken Tok_In <|> pToken Tok_FlowsInto

-- | Parse a let context.
--
--   This should change soon to just ask for the Let, then call
--   pDecls.
--
--   The AST needs to change a bit though for that to happen.
pContextLet :: Parser s m => m [Context Range Var]
pContextLet = do
  p <- pToken Tok_Let
  _ <- pToken Tok_LBrace

  let
    letFun = do
      (_, n) <- pVariable
      Let p (PatVariable n) <$> pFunction

    letPat = do
      pat    <- pPattern
      _      <- pToken Tok_Equals
      Let p pat <$> pExp

    letE   = letFun <|> letPat

  ret <- letE  `Mega.sepEndBy1` pToken Tok_Semi
  _   <- pToken Tok_RBrace
  return ret


-- Just singular contexts here, not lets, which
-- expand into multiple.
-- This will be the whole thing when lets are changed
-- to take a list of Decls.
pSingleContext :: Parser s m => m (Context Range Var)
pSingleContext =
  choice [
      pContextWindowed
    , pContextGroup
    , pContextDistinct
    , pContextFilter
    , pContextLatest
    , pContextFold
    ]


pContextGroup :: Parser s m => m (Context Range Var)
pContextGroup = do
  pos <- pToken Tok_Group
  pContextGroupBy pos <|> pContextGroupFold pos


pContextGroupBy :: Parser s m => Range -> m (Context Range Var)
pContextGroupBy pos =
  GroupBy pos
    <$> pExp


pContextGroupFold :: Parser s m => Range -> m (Context Range Var)
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


pContextDistinct :: Parser s m => m (Context Range Var)
pContextDistinct =
  Distinct
    <$> pToken Tok_Distinct
    <*> pExp


pContextFilter :: Parser s m => m (Context Range Var)
pContextFilter =
  Filter
    <$> pToken Tok_Filter
    <*> pExp


pContextLatest :: Parser s m => m (Context Range Var)
pContextLatest =
  Latest
    <$> pToken Tok_Latest
    <*> fmap (fromInteger . snd) pInteger


pContextWindowed :: Parser s m => m (Context Range Var)
pContextWindowed = do
  pos <- pToken Tok_Windowed
  pContextBetween pos <|> pContextAfter pos


pContextAfter :: Parser s m => Range -> m (Context Range Var)
pContextAfter pos =
  Windowed pos
    <$> pWindowSizeUnit
    <*> pure Nothing


pContextBetween :: Parser s m => Range -> m (Context Range Var)
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
pContextFold :: Parser s m => m (Context Range Var)
pContextFold = do
  (p, ft) <- pFoldType
  n       <- pPattern
  _       <- pToken Tok_Equals
  z       <- pQuery                                         <?> "initial value"
  _       <- pToken Tok_Then                                <?> "then"
  k       <- pQuery                                         <?> "fold expression"
  return $ LetFold p (Fold n (simpNested z) (simpNested k) ft)


pFoldType :: Parser s m => m (Range, FoldType)
pFoldType
    =   (pToken Tok_Fold1 `with` (, FoldTypeFoldl1))
    <|> (pToken Tok_Fold  `with` (, FoldTypeFoldl))


pPattern :: Parser s m => m (Pattern Var)
pPattern
 = do e <- pExp
      checkPat e


pExp :: Parser s m => m (Exp Range Var)
pExp = do
  o  <- Mega.getOffset
  xs <- some ((Left <$> pExp1) <|> pOp) <?> "expression"
  either (failAtOffset o . renderDefixError) return (defix xs)

 where
  pOp = do (p, o) <- pVarOp
           return (Right (o,p))

pExp1 :: Parser s m => m (Exp Range Var)
pExp1
 =   ((uncurry Var <$> var       ) >>= accessor)
 <|> parseRecord
 <|> (uncurry Prim <$> primitives)
 <|> inParens
 <|> parseIf
 <|> parseCase
 <?> "expression"
 where
  var
   = pVariable

  field =
    tryToken $ \_ -> \case
      Tok_VarId prjId ->
        Just $ StructField prjId
      _ ->
        Nothing


  accessor v
    = (accessor1 v >>= accessor)
   <|> pure v

  accessor1 v
   = tryToken $ \pos -> \case
      Tok_PrjId prjId ->
        Just $ Access pos v (StructField prjId)
      _ ->
        Nothing

  simpQuery =
    simpNested <$> pQuery

  tuples r =
    let
      build [] = Prim r (PrimCon ConUnit)
      build (x:xs) =
        foldl (\a b -> mkApp (mkApp (Prim (annotOfExp a) $ PrimCon ConTuple) a) b) x xs
    in
      build <$> Mega.sepBy simpQuery (pToken Tok_Comma)

  inParens
   = (pToken Tok_LParen >>= tuples) <* pToken Tok_RParen <?> "sub-expression or nested query"

  parseIf
   = do pos   <- pToken Tok_If
        scrut <- pExp
        _     <- pToken Tok_Then
        true  <- simpQuery
        _     <- pToken Tok_Else
        false <- simpQuery
        return $ If pos scrut true false

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
        xx  <- simpQuery
        return (pat, xx)

  parseRecord
   = do pos   <- pToken Tok_LBrace
        flds  <- parseField `Mega.sepEndBy` pToken Tok_Comma
        _     <- pToken Tok_RBrace
        return $ Record pos flds

  parseField
   = do fld <- field
        _   <- pToken Tok_Equals
        xx  <- simpQuery
        return (fld, xx)


pUnresolvedInputId :: Parser s m => m UnresolvedInputId
pUnresolvedInputId
 = tryToken get <?> "input identifier"
 where
  get _ (Tok_VarId v)  = parseUnresolvedInputId v
  get _ (Tok_ConId v)  = parseUnresolvedInputId v
  get _ (Tok_String v) = parseUnresolvedInputId v
  get _ _              = Nothing


primitives :: Parser s m => m (Range, Prim)
primitives
 =   (second (Lit . LitInt . fromInteger)      <$> pInteger)
 <|> (second (Lit . LitDouble . toRealFloat)   <$> pRational)
 <|> (second (Lit . LitString)                 <$> pString)
 <|> (second (Lit . LitTime . midnight . Date) <$> pDate)
 <|> second PrimCon                            <$> pConstructor
 <|> second Fun                                <$> parseRegex
 <|> timePrimitives
 <?> "primitive"


parseRegex :: Parser s m => m (Range, BuiltinFun)
parseRegex
  = do p         <- pToken Tok_Grepl
       o         <- Mega.getOffset
       (_,s)     <- pString
       let mRegex = Mega.parseMaybe Regex.parser s
       regex     <- maybe (failAtOffset o "Not a valid regular expression") pure mRegex
       return (p, BuiltinRegex (Grepl s regex))


pConstructor :: Parser s m => m (Range, Constructor)
pConstructor
 = do o                <- Mega.getOffset
      (p, Construct n) <- pConId
      case List.lookup n constructors of
        Just c -> return (p, c)
        Nothing -> failAtOffset o ("Not a known constructor: " <> show n)



timePrimitives :: Parser s m => m (Range, Prim)
timePrimitives
 =   (, Fun (BuiltinTime DaysJulianEpoch)) <$> pToken Tok_Days
 <|> (, Fun (BuiltinTime SecondsJulianEpoch)) <$> pToken Tok_Seconds


pWindowSizeUnit :: Parser s m => m Common.WindowUnit
pWindowSizeUnit
 = do (_, i) <- second fromInteger <$> pInteger <?> "window amount"
      unit Tok_Days (Common.Days i)
        <|> unit Tok_Months (Common.Months i)
        <|> unit Tok_Weeks (Common.Weeks i)

 where
  unit kw q
   = q <$ pToken kw


simpNested :: Query a n -> Exp a n
simpNested q = case q of
  Query [] e
    -> e
  contextual
    -> Nested (annotOfQuery contextual) contextual
