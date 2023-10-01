{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
module Icicle.Source.Transform.Desugar
  ( DesugarError(..)
  , annotOfError
  , runDesugar
  , desugarQT
  , desugarQ
  , desugarX
  ) where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Either

import qualified Data.Map as Map
import           Data.Hashable (Hashable)
import           Data.Functor.Identity

import           GHC.Generics (Generic)

import           Icicle.Common.Base
import           Icicle.Common.Fresh

import           Icicle.Source.Query
import           Icicle.Source.Transform.Simp
import           Icicle.Source.Transform.SubstX
import           Icicle.Internal.Pretty

import           P
import qualified Data.List as List



data DesugarError a n
 = DesugarErrorNoAlternative a (Pattern n) -- ^ we generated a pattern that cannot be matched
                                           --   with any alternative.
 | DesugarErrorImpossible a                -- ^ just impossible, the world has ended.
 | DesugarOverlappingPattern a (Pattern n) -- ^ duh
 | DesugarIllTypedPatterns   a [Pattern n] -- ^ patterns use constructors from different types
 deriving (Eq, Show, Generic)

instance (NFData a, NFData n) => NFData (DesugarError a n)

instance (Pretty a, Pretty n) => Pretty (DesugarError a n) where
  pretty (DesugarErrorNoAlternative a n) = "Missing alternative:" <+> pretty n <+> "at" <+> pretty a
  pretty (DesugarErrorImpossible a)      = "Impossible desugar error" <+> "at" <+> pretty a
  pretty (DesugarOverlappingPattern a x) = "Overlapping pattern:" <+> pretty x <+> "at" <+> pretty a
  pretty (DesugarIllTypedPatterns a xs)  = "Illtyped patterns:"   <+> align (vcat (pretty <$> xs)) <> line <> "at" <+> pretty a

type DesugarM a n x = FreshT n (EitherT (DesugarError a n) Identity) x

annotOfError :: DesugarError a n -> a
annotOfError (DesugarErrorNoAlternative a _) = a
annotOfError (DesugarErrorImpossible a)      = a
annotOfError (DesugarOverlappingPattern a _) = a
annotOfError (DesugarIllTypedPatterns a _)   = a

runDesugar :: NameState n -> DesugarM a n x -> Either (DesugarError a n) x
runDesugar n m = runIdentity . runEitherT . bimapEitherT id snd $ runFreshT m n

desugarQT
  :: (Hashable n, Eq n)
  => QueryTop a n
  -> DesugarM a n (QueryTop a n)
desugarQT qt
  = do qq' <- desugarQ (query qt)
       return $ qt { query = qq' }

desugarQ
  :: (Hashable n, Eq n)
  => Query a n
  -> DesugarM a n (Query a n)
desugarQ qq
  = do ex <- concat <$> mapM desugarLets (contexts qq)
       cs <- mapM desugarC ex
       f  <- desugarX (final qq)
       return $ Query cs f

-- | Rewrite let and fold binding such that we only
--   match on a single PatVariable at a time.
--   Pairs get rewritten, such that
--   let (x, _) = (1, 2)
--   will become
--   let n = (1,2) ~> let x = fst n
desugarLets
  :: (Hashable n, Eq n)
  => Context a n
  -> DesugarM a n [Context a n]
desugarLets cc
 = case cc of
    Let _ PatDefault _
      -> return []

    Let a (PatCon ConTuple [apat, bpat]) x
      -> do n        <- fresh
            let nbind = Let a (PatVariable n) x
            abind    <- Let a apat <$> from n fst
            bbind    <- Let a bpat <$> from n snd
            ccs      <- mapM desugarLets [nbind, abind, bbind]
            return $ concat ccs
          where
            from n which = do
              b@(i,j)  <- (,) <$> fresh <*> fresh
              let
                tup   = PatCon ConTuple [PatVariable i, PatVariable j]
                varn  = (Var a n)
              return $ Case a varn [(tup, Var a (which b))]

    Let a (PatRecord fields) x
      -> do n        <- fresh
            let nbind = Let a (PatVariable n) x
            let bound = flip fmap fields $ \(field, pat) ->
                          Let a pat (Access a (Var a n) field)
            ccs      <- mapM desugarLets (nbind : bound)
            return $ concat ccs

    LetFold a f
      | PatDefault <- foldBind f
      -> return []
      | PatCon {}  <- foldBind f
      -> do n        <- fresh
            let nbind = Let a (foldBind f) (Var a n)
            nnbind   <- desugarLets nbind
            return $
              LetFold a f {
                foldBind = PatVariable n
              , foldWork = Nested a (Query [nbind] (foldWork f))
              } : nnbind

    GroupFold a k v m
      -> let
           expand PatDefault = do
             n <- fresh
             return (PatVariable n, [])
           expand (PatVariable n) =
             return (PatVariable n, [])
           expand c = do
             n <- fresh
             let nbind = Let a c (Var a n)
             return (PatVariable n, [nbind])
         in do
           (k', kx) <- expand k
           (v', vx) <- expand v
           lets     <- mapM desugarLets (kx <> vx)
           return $
             GroupFold a k' v' m : concat lets

    LetScan a n x
      -> let
           expand PatDefault = do
             n' <- fresh
             return (PatVariable n', [])
           expand (PatVariable n') =
             return (PatVariable n', [])
           expand c = do
             n' <- fresh
             let nbind = Let a c (Var a n')
             return (PatVariable n', [nbind])
         in do
           (n', nx) <- expand n
           lets     <- mapM desugarLets nx
           return $
             LetScan a n' x : concat lets

    FilterLet a (PatCon ConRight [arg]) x
      -> do n        <- fresh
            i        <- fresh
            j        <- fresh
            let nbind = Let a (PatVariable n) $ Case a x [(PatCon ConRight [PatVariable i], con1 a ConSome (Var a i))
                                                         ,(PatCon ConLeft  [PatVariable j], con0 a ConNone)]
            let more  = FilterLet a (PatCon ConSome [arg]) (Var a n)
            ccs      <- mapM desugarLets [nbind, more]
            return $ concat ccs

    FilterLet a (PatCon ConLeft [arg]) x
      -> do n        <- fresh
            i        <- fresh
            j        <- fresh
            let nbind = Let a (PatVariable n) $ Case a x [(PatCon ConLeft  [PatVariable i], con1 a ConSome (Var a i))
                                                         ,(PatCon ConRight [PatVariable j], con0 a ConNone)]
            let more  = FilterLet a (PatCon ConSome [arg]) (Var a n)
            ccs      <- mapM desugarLets [nbind, more]
            return $ concat ccs

    FilterLet a (PatCon ConSome [subCases]) x
      -> do n        <- fresh
            let nbind = FilterLet a subCases (Var a n)
            nnbind   <- desugarLets nbind
            return $
              FilterLet a (PatCon ConSome [PatVariable n]) x : nnbind

    FilterLet a (PatCon ConNone []) x
      -> do nSome    <- fresh
            return . pure $ Filter a $
              Case a x [(PatCon ConSome [PatVariable nSome], Prim a (PrimCon ConFalse)),
                        (PatCon ConNone [],                  Prim a (PrimCon ConTrue))]

    FilterLet ann (PatLit lit negated) scrut
      -> do let eq = Prim ann $ Op $ Relation Eq
                neg = Prim ann $ Op $ ArithUnary Negate
                lit' = Prim ann $ Lit lit
                prim = if negated then App ann neg lit' else lit'
                sc = App ann (App ann eq scrut) prim
            return . pure $ Filter ann sc

    FilterLet ann (PatCon ConTuple [apat, bpat]) x
      -> do n        <- fresh
            let nbind = Let ann (PatVariable n) x
            abind    <- FilterLet ann apat <$> from n fst
            bbind    <- FilterLet ann bpat <$> from n snd
            ccs      <- mapM desugarLets [nbind, abind, bbind]
            return $ concat ccs
          where
            from n which = do
              b@(i,j)  <- (,) <$> fresh <*> fresh
              let
                tup   = PatCon ConTuple [PatVariable i, PatVariable j]
                varn  = (Var ann n)
              return $ Case ann varn [(tup, Var ann (which b))]

    FilterLet a (PatRecord fields) x
      -> do n        <- fresh
            let nbind = Let a (PatVariable n) x
            let bound = flip fmap fields $ \(field, pat) ->
                          FilterLet a pat (Access a (Var a n) field)
            ccs      <- mapM desugarLets (nbind : bound)
            return $ concat ccs

    FilterLet a simpleBinding x
      -> do desugarLets $ Let a simpleBinding x

    x -> return [x]

desugarC
  :: (Hashable n, Eq n)
  => Context a n
  -> DesugarM a n (Context a n)
desugarC cc
 = case cc of
    GroupBy   a   x   -> GroupBy   a     <$> desugarX x
    Distinct  a   x   -> Distinct  a     <$> desugarX x
    Filter    a   x   -> Filter    a     <$> desugarX x
    FilterLet a n x   -> FilterLet a n   <$> desugarX x
    Let       a n x   -> Let       a n   <$> desugarX x
    LetScan   a n x   -> LetScan   a n   <$> desugarX x
    LetFold   a   f   -> LetFold   a     <$> desugarF f
    ArrayFold a v x   -> ArrayFold a v   <$> desugarX x
    GroupFold a k v x -> GroupFold a k v <$> desugarX x
    Windowed{}        -> return cc
    Latest{}          -> return cc

desugarF
  :: (Hashable n, Eq n)
  => Fold Query a n
  -> DesugarM a n (Fold Query a n)
desugarF ff
  = do fi' <- desugarX (foldInit ff)
       fw' <- desugarX (foldWork ff)
       return $ ff { foldInit = fi', foldWork = fw'}

desugarX
  :: (Hashable n, Eq n)
  => Exp a n
  -> DesugarM a n (Exp a n)
desugarX xx
 = case xx of
    Nested a q
     -> do q' <- desugarQ q
           return $ Nested a q'

    Case a scrut patalts
     -> do let pats  = fmap fst patalts
           ty       <- foldM (flip $ addToTy $ DesugarIllTypedPatterns a pats) TyAny pats

           scrut'   <- desugarX scrut
           patalts' <- mapM (mapM desugarX) patalts

           tree     <- casesForTy a scrut' ty
           checkOverlapping a pats (toList tree)

           treeToCase a patalts' tree

    -- Beta Reduction
    App _ (Lam _ n x1) x2
      -> do x1' <- desugarX x1
            x2' <- desugarX x2
            xx' <- runFreshIdentity $ substX (Map.singleton n x2') x1'
            return xx'

    App a x1 x2
      -> do x1' <- desugarX x1
            x2' <- desugarX x2
            return $ App a x1' x2'

    If a x1 x2 x3
      -> do x1' <- desugarX x1
            x2' <- desugarX x2
            x3' <- desugarX x3
            return $ If a x1' x2' x3'

    Lam a n x1
      -> do x1' <- desugarX x1
            return $ Lam a n x1'

    Var _ _
      -> return xx
    Prim _ _
     -> return xx

    Access a x f
      -> do x' <- desugarX x
            return $ Access a x' f
    Record a fs
      -> do fs' <- traverse (traverse desugarX) fs
            return $ Record a fs'

--------------------------------------------------------------------------------

-- * Case Flattening

-- | The partial "type" of patterns, up to where they are matched.
--   The type of the scrutinee is strictly more specific than this, but we don't
--   want to generate too many cases.
--
data Ty
 = TyTup Ty Ty
 | TyOpt Ty
 | TySum Ty Ty
 | TyBool
 | TyUnit
 -- Literals such as numbers and argument-less enums:
 -- where we can, instead of doing a real case, check against "x == Con".
 -- This doesn't work for Bool because this desugaring uses Bool
 | TyLit [TyLit]

 | TyRecord [(StructField, Ty)]
 | TyAny
 deriving (Show)

data TyLit
 = TyLitCon Constructor
 | TyLitLit Lit Bool
 deriving (Show)

addToTy :: DesugarError a n -> Pattern n -> Ty -> DesugarM a n Ty
addToTy err (PatCon con pats) ty
 = case con of
    ConTuple
     | [p1, p2]         <- pats
     , TyTup t1 t2      <- ty
     -> TyTup <$> go p1 t1    <*> go p2 t2
     | [p1, p2]         <- pats
     , TyAny            <- ty
     -> TyTup <$> go p1 TyAny <*> go p2 TyAny
     | otherwise
     -> lift $ left err

    ConSome
     | [p]              <- pats
     , TyOpt t          <- ty
     -> TyOpt <$> go p t
     | [p]              <- pats
     , TyAny            <- ty
     -> TyOpt <$> go p TyAny
     | otherwise
     -> lift $ left err

    ConNone
     | []               <- pats
     , TyOpt _          <- ty
     -> return ty
     | []               <- pats
     , TyAny            <- ty
     -> return $ TyOpt TyAny
     | otherwise
     -> lift $ left err

    ConTrue
     | []               <- pats
     , TyBool           <- ty
     -> return ty
     | []               <- pats
     , TyAny            <- ty
     -> return TyBool
     | otherwise
     -> lift $ left err

    ConFalse
     | []               <- pats
     , TyBool           <- ty
     -> return ty
     | []               <- pats
     , TyAny            <- ty
     -> return TyBool
     | otherwise
     -> lift $ left err

    ConLeft
     | [p]              <- pats
     , TySum t1 t2      <- ty
     -> TySum <$> go p t1    <*> return t2
     | [p]              <- pats
     , TyAny            <- ty
     -> TySum <$> go p TyAny <*> return TyAny
     | otherwise
     -> lift $ left err

    ConRight
     | [p]              <- pats
     , TySum t1 t2      <- ty
     -> TySum <$> return t1    <*> go p t2
     | [p]              <- pats
     , TyAny            <- ty
     -> TySum <$> return TyAny <*> go p TyAny
     | otherwise
     -> lift $ left err

    ConUnit
     | []               <- pats
     , TyUnit           <- ty
     -> return ty
     | []               <- pats
     , TyAny            <- ty
     -> return TyUnit
     | otherwise
     -> lift $ left err

    ConError _
     | []               <- pats
     , TyLit cs         <- ty
     -> return $ TyLit (cs <> [TyLitCon con])
     | []               <- pats
     , TyAny            <- ty
     -> return $ TyLit [TyLitCon con]
     | otherwise
     -> lift $ left err
 where
  go = addToTy err

addToTy err (PatLit l n) ty = case ty of
  TyAny    -> return $ TyLit [TyLitLit l n]
  TyLit ls -> return $ TyLit (ls <> [TyLitLit l n])
  _        -> lift $ left err


addToTy _ PatDefault      ty    = return ty
addToTy _ (PatVariable _) ty    = return ty

addToTy err r@(PatRecord fields) TyAny =
  let a = fmap (fmap (const TyAny)) fields
  in addToTy err r $ TyRecord a

addToTy err (PatRecord fields) (TyRecord tys) =
  let goer (n, p) (_, ty) = (n,) <$> addToTy err p ty
  in  TyRecord <$> zipWithM goer fields tys

addToTy err (PatRecord _) _ =
  lift $ left err

casesForTy
  :: (Hashable n)
  => a
  -> Exp a n
  -> Ty
  -> DesugarM a n (Tree a n (Pattern n))
casesForTy ann scrut ty
 = case ty of

    -- Booleans just have True/False
    TyBool
     -> return
      $ TCase scrut [ (PatCon ConTrue  [], Done (PatCon ConTrue  []))
                    , (PatCon ConFalse [], Done (PatCon ConFalse [])) ]

    -- Tuples treat arguments as nested cases
    TyTup a b
     -> do args     <- freshes 2
           let pat'  = PatCon ConTuple (fmap PatVariable args)
           let vars  = fmap (Var ann) args
           bd       <- subtree ConTuple vars [a, b]
           return $ TCase scrut [ (pat', bd) ]

    -- Options need a case for None, and nested cases for Some arguments
    TyOpt a
     -> do args     <- freshes 1
           let pat'  = PatCon ConSome (fmap PatVariable args)
           let vars  = fmap (Var ann) args
           bd       <- subtree ConSome vars [a]
           return $ TCase scrut [ (pat', bd)
                               , (PatCon ConNone [], Done (PatCon ConNone [])) ]

    -- Sums need nested cases for both
    TySum a b
     -> do aleft     <- freshes 1
           let pleft  = PatCon ConLeft  (fmap PatVariable aleft)
           aright    <- freshes 1
           let pright = PatCon ConRight (fmap PatVariable aright)
           let vars   = fmap (Var ann)
           bl <- subtree ConLeft  (vars aleft)  [a]
           br <- subtree ConRight (vars aright) [b]
           return $ TCase scrut [ (pleft,  bl)
                                , (pright, br) ]

    TyLit cs
     -> do var <- fresh
           let
             pFor x = case x of
               TyLitCon c -> PatCon c []
               TyLitLit l n -> PatLit l n
           return $ TLet var scrut
                  $ TLits   (Var ann var)
                            (fmap (\c -> (c, Done $ pFor c)) cs)
                            (Done $ PatVariable var)

    -- Unit just has the unit constructor
    TyUnit
     -> return
      $ TCase scrut [ (PatCon ConUnit  [], Done (PatCon ConUnit  [])) ]

    TyRecord fields
     -> do let (names, tys) = List.unzip fields
           args     <- traverse (\n -> (n,) <$> fresh) names
           let pat'  = PatRecord  (second PatVariable <$> args)
           let vars  = fmap (Var ann . snd) args
           bds      <- zipWithM (casesForTy ann) vars tys
           let bd    = fmap (PatRecord . List.zip names) $ sequence bds
           return $ TCase scrut [ (pat', bd) ]

    -- If we don't know the type of this pattern, use a fresh variable
    -- Use TLet to avoid generating variable patterns, since Core can't handle them.
    TyAny
     -> do var <- fresh
           return $ TLet var scrut (Done (PatVariable var) )

 where
  subtree con vars tys
   = liftM (fmap (PatCon con) . sequence)
   $ zipWithM (casesForTy ann) vars tys

-- | A nested case AST. We generate this from the patterns and convert it into
--   a case statement.
--
data Tree a n x
 = Done  x                         -- ^ just use the pattern/alternative/thing.
 | TCase (Exp a n)
         [(Pattern n, Tree a n x)] -- ^ do a case statement
 | TLet  (Name n)
         (Exp a n)
         (Tree a n x)              -- ^ insert a let because we cannot generate pattern variables.
 | TLits (Exp a n)
         [(TyLit, Tree a n x)] (Tree a n x) -- ^ special case for literals
 deriving (Functor, Foldable, Traversable, Show)


instance Applicative (Tree a n) where
  pure  = Done
  (<*>) = ap

instance Monad (Tree a n) where
  return  = pure
  a >>= b = joinT (fmap b a)
   where
    joinT (Done x)     = x
    joinT (TCase n ls) = TCase n (fmap (fmap joinT) ls)
    joinT (TLet n x t) = TLet  n x (joinT t)
    joinT (TLits  s cs d)
                       = TLits s (fmap (fmap joinT) cs) (joinT d)


treeToCase
  :: (Eq n)
  => a
  -> [(Pattern n, Exp a n)]
  -> Tree a n (Pattern n)
  -> DesugarM a n (Exp a n)
treeToCase ann patalts tree
 = lift . fmap (simpDumbX . caseStmtsFor) . sequence
 $ fmap (getAltBody patalts) tree
  where
   -- Convert tree structure to AST
   caseStmtsFor (Done x)
    = x
   caseStmtsFor (TCase scrut alts)
    = Case ann scrut (fmap (fmap caseStmtsFor) alts)
   caseStmtsFor (TLet n x e)
    = Nested ann (Query [Let ann (PatVariable n) x] (caseStmtsFor e))

   caseStmtsFor (TLits scrut ((c,x):cs) d)
    = let eq = Prim ann $ Op $ Relation Eq
          neg = Prim ann $ Op $ ArithUnary Negate
          prim = case c of
            TyLitLit l True -> App ann neg $ Prim ann $ Lit l
            TyLitLit l False -> Prim ann $ Lit l
            TyLitCon c' -> Prim ann $ PrimCon c'
          sc = App ann (App ann eq scrut) prim
      in Case ann sc
            [ ( PatCon ConTrue  [], caseStmtsFor x )
            , ( PatCon ConFalse [], caseStmtsFor $ TLits scrut cs d ) ]
   caseStmtsFor (TLits _ [] d)
    = caseStmtsFor d

   -- Look up the alternative for this pattern
   getAltBody ((px, x) : xs) p
    = case matcher p px of
       Nothing
        -> getAltBody xs p
       Just []
        -> right x
       Just s
        -> do s' <- mapM generateLet s
              right $ Nested ann (Query s' x)
   getAltBody _ p
    = left $ DesugarErrorNoAlternative ann p

   generateLet (n ,p)
    = Let ann (PatVariable n) <$> patternToExp p

   patternToExp (PatCon c as)
    = do xs <- mapM patternToExp as
         right $ foldl' (App ann) (Prim ann (PrimCon c)) xs
   patternToExp (PatRecord fields)
    = Record ann <$> traverse (traverse patternToExp) fields
   patternToExp (PatVariable v)
    = right $ Var ann v
   patternToExp (PatLit l True)
    = right $ Prim ann (Lit l)
   patternToExp (PatLit l False)
    = let neg = Prim ann $ Op $ ArithUnary Negate
      in  right $ App ann neg $ Prim ann (Lit l)
   patternToExp PatDefault
    = left $ DesugarErrorImpossible ann -- we never generate default patterns.

-- "Unify" the generated pattern and a user-supplied pattern.
-- Return a list of substitutions if success. This is necessary in case the
-- generated patterns are more specific than the user's pattern, e.g.
-- if we have `None` and the user just supplies a variable.
--
-- Precondition: matcher p q assumes p is more specific than q
-- Precondition: matcher p q assumes p contains no PatDefault
matcher :: Pattern t -> Pattern t -> Maybe [(Name t, Pattern t)]
matcher (PatCon c as) (PatCon c' bs)
 = do guard (c == c')
      substs <- zipWithM matcher as bs
      return (concat substs)
matcher p (PatVariable x)
 = return [(x, p)]
matcher _ (PatDefault)
 = return []
matcher (PatLit l n) (PatLit l' n')
 | l == l'
 , n == n'
 = return []
matcher (PatRecord as) (PatRecord bs)
 = do substs <- sequence $ Map.elems $ Map.intersectionWith matcher (Map.fromList as) (Map.fromList bs)
      return (concat substs)
matcher _ _
 = Nothing


checkOverlapping
  :: a -> [Pattern n] -> [Pattern n] -> DesugarM a n ()
checkOverlapping ann userpats genpats
  = foldM_ (\p -> lift . go p) genpats userpats
  where
   go gps up
    = let gps' = filter (\gp -> isNothing $ matcher gp up ) gps
      in  if length gps' < length gps
          then return gps'
          else left (DesugarOverlappingPattern ann up)


freshes :: (Monad m, Hashable n) => Int -> FreshT n m [Name n]
freshes n = replicateM n fresh


con0 :: a -> Constructor -> Exp a n
con0 a c   =        Prim a (PrimCon c)

con1 :: a -> Constructor -> Exp a n -> Exp a n
con1 a c x = App a (Prim a (PrimCon c)) x
