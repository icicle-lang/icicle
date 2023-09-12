-- | Top-level queries
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
module Icicle.Source.Query.Query (
    QueryTop  (..)
  , Query     (..)
  , Exp
  , Context

  , simplifyNestedQT
  , simplifyNestedQ
  , simplifyNestedC
  , simplifyNestedX

  , annotOfQuery

  , allvarsX, allvarsC, allvarsQ, allvarsP
  ) where

import qualified Data.Set                as Set

import           GHC.Generics (Generic)

import           Icicle.Common.Base
import           Icicle.Data.Name
import           Icicle.Internal.Pretty
import           Icicle.Source.Query.Constructor
import           Icicle.Source.Query.Context
import           Icicle.Source.Query.Exp

import           P


data QueryTop a n
 = QueryTop
 { queryInput :: UnresolvedInputId
 , queryName  :: OutputId
 , query      :: Query a n }
 deriving (Show, Eq, Ord, Generic)

instance (NFData a, NFData n) => NFData (QueryTop a n)



-- | An Icicle Query
--
--   Queries can be thought of as an expression in
--   a context. In many languages, the only context
--   of this form is a let binding, and they are
--   inlined into the expression data type.
--
--   In icicle, we support more contexts to support
--   rich streaming queries, and contexts are separated
--   into their own type.
data Query a n
 = Query
 { contexts :: [Context a n]
 , final    :: Exp a n }
 deriving (Show, Eq, Ord, Generic)

instance (NFData a, NFData n) => NFData (Query a n)

-- | "Tie the knot" so expressions can have nested queries.
-- See Exp.
type Exp     a n = Exp'     Query a n
type Context a n = Context' Query a n

instance Pretty n => Pretty (QueryTop a n) where
  pretty q =
    vsep [
        prettyKeyword "from" <+> annotate AnnConstant (pretty (show (renderUnresolvedInputId (queryInput q))))
      , prettyPunctuation "in" <+> align (pretty (query q))
      ]

instance Pretty n => Pretty (Query a n) where
  pretty q =
    align . prettyItems vsep (align . pretty $ final q) $
      fmap (PrettyItem (prettyPunctuation "in") . align . pretty) (contexts q)


simplifyNestedQT :: QueryTop a n -> QueryTop a n
simplifyNestedQT q
 = q { query = simplifyNestedQ $ query q }

simplifyNestedQ :: Query a n -> Query a n
simplifyNestedQ q
 = simp
 $ Query (fmap simplifyNestedC $ contexts q)
         (     simplifyNestedX $ final    q)
 where
  simp q'
   | Query cs (Nested _ (Query cs' x')) <- q'
   = Query (cs <> cs') x'
   | otherwise
   = q'

simplifyNestedC :: Context a n -> Context a n
simplifyNestedC c
 = case c of
    Windowed{} ->
      c
    Latest{} ->
      c
    GroupBy a x ->
      GroupBy  a $ simplifyNestedX x
    GroupFold a k v x ->
      GroupFold a k v $ simplifyNestedX x
    Distinct a x ->
      Distinct a $ simplifyNestedX x
    Filter a x ->
      Filter a $ simplifyNestedX x
    FilterLet a p x ->
      FilterLet a p $ simplifyNestedX x
    LetFold  a f ->
      LetFold a f {
        foldInit = simplifyNestedX $ foldInit f,
        foldWork = simplifyNestedX $ foldWork f
      }
    Let a n x ->
      Let a n $ simplifyNestedX x
    LetScan a n x ->
      LetScan a n $ simplifyNestedX x


simplifyNestedX :: Exp a n -> Exp a n
simplifyNestedX xx
 = case xx of
    Nested _ (Query [] x)
     -> simplifyNestedX x

    Nested a q
     -> Nested a $ simplifyNestedQ q

    App a x y
     -> App a (simplifyNestedX x) (simplifyNestedX y)

    Lam a n x
     -> Lam a n (simplifyNestedX x)

    Var{}
     -> xx

    Prim{}
     -> xx

    If a p t f
     -> If a (simplifyNestedX p) (simplifyNestedX t) (simplifyNestedX f)

    Case a scrut pats
     -> Case a (simplifyNestedX scrut)
      $ fmap (\(p,x) -> (p, simplifyNestedX x)) pats

    Access a x f
     -> Access a (simplifyNestedX x) f

    Record a fields
     -> Record a
      $ fmap (\(p,x) -> (p, simplifyNestedX x)) fields

instance TraverseAnnot Query  where
  traverseAnnot f q =
    Query <$> traverse (traverseAnnot f) (contexts q)
          <*>           traverseAnnot f  (final    q)

instance TraverseAnnot QueryTop  where
  traverseAnnot f (QueryTop i0 n0 q0)
    = QueryTop i0 n0 <$> traverseAnnot f q0

annotOfQuery :: Query a n -> a
annotOfQuery q
 = case contexts q of
    []    -> annotOfExp $ final q
    (c:_) -> annotOfContext c


-- | Compute set of all value variables of queries
-- (As opposed to type variables; for that see freeOfAllQ in Checker/Constraint)
allvarsQ :: Eq n => Query a n -> Query (a, Set.Set (Name n)) n
allvarsQ (Query cs x)
 = let x'  = allvarsX x
       cs' = goCs x'
   in  Query cs' x'
 where
  goCs x'
   = reverse
   $ goCs' (snd $ annotOfExp x')
   $ reverse cs

  goCs' _ [] = []
  goCs' ns (c:cs')
   = let c'  = allvarsC ns c
         ns' = Set.union ns (snd $ annotOfContext c')
     in  c' : goCs' ns' cs'

-- | Compute set of value variables of context
-- Because the context isn't a thing on its own, it needs the set of variables
-- used in the rest of the query.
allvarsC :: Eq n => Set.Set (Name n) -> Context a n -> Context (a, Set.Set (Name n)) n
allvarsC ns c
 = case c of
    Windowed a w1 w2
     -> Windowed (a,ns) w1 w2
    Latest a w1
     -> Latest (a,ns) w1

    Let a p x
     -> let x' = allvarsX x
            (p',nsX) = allvarsP p
            ns' = Set.unions
                [ ns, nsX, annX x' ]
        in  Let (a, ns') p' x'

    LetScan a p x
     -> let x' = allvarsX x
            (p',nsX) = allvarsP p
            ns' = Set.unions
                [ ns, nsX, annX x' ]
        in  LetScan (a, ns') p' x'

    LetFold a f
     -> let z'  = allvarsX (foldInit f)
            k'  = allvarsX (foldWork f)
            (p',nsX) = allvarsP (foldBind f)
            ns' = Set.unions
                [ ns, nsX
                , annX z', annX k' ]
            f'  = f { foldInit = z', foldWork = k', foldBind = p' }
        in  LetFold (a,ns') f'

    GroupBy a x
     -> let x'  = allvarsX x
            ns' = Set.union ns (annX x')
        in  GroupBy (a,ns') x'

    GroupFold a nk nv x
     -> let x'  = allvarsX x
            (nk',nkX) = allvarsP nk
            (nv',nvX) = allvarsP nv
            ns' = Set.unions
                [ ns, nkX, nvX, annX x' ]
        in  GroupFold (a,ns') nk' nv' x'

    Distinct a x
     -> let x'  = allvarsX x
            ns' = Set.union ns (annX x')
        in  Distinct (a,ns') x'

    Filter a x
     -> let x'  = allvarsX x
            ns' = Set.union ns (annX x')
        in  Filter (a,ns') x'

    FilterLet a p x
     -> let x'       = allvarsX x
            (p',nsX) = allvarsP p
            ns'      = Set.unions [ ns, nsX, annX x' ]
        in  FilterLet (a,ns') p' x'

 where
  annX = snd . annotOfExp


-- | Compute set of value variables of expression
allvarsX :: Eq n => Exp a n -> Exp (a, Set.Set (Name n)) n
allvarsX x
 = case x of
    Var a n
     -> Var (a, sgl n) n
    Lam a n p
     -> let p' = allvarsX p
         in Lam (a, Set.delete n (annX p')) n p'
    Nested a q
     -> let q' = allvarsQ q
         in Nested (a, snd $ annotOfQuery q') q'
    App a p q
     -> let p' = allvarsX p
            q' = allvarsX q
         in App (a, Set.union (annX p') (annX q')) p' q'
    Prim a p
     -> Prim (a, Set.empty) p
    If a p t f
     -> let p' = allvarsX p
            t' = allvarsX t
            f' = allvarsX f
         in If (a, Set.unions [annX p', annX t', annX f']) p' t' f'
    Case a s ps
     -> let s'        = allvarsX s
            (ps',ns') = goPatXs ps
         in Case (a, Set.union (annX s') ns') s' ps'
    Access a e f
     -> let e' = allvarsX e
         in Access (a, annX e') e' f
    Record a fs
     -> let fs'       = second allvarsX <$> fs
         in Record (a, Set.unions (annX . snd <$> fs')) fs'

 where
  annX = snd . annotOfExp
  sgl  = Set.singleton

  goPatXs []
   = ([], Set.empty)
  goPatXs ((p,xx):ps)
   = let (ps',ns') = goPatXs  ps
         (p',np')  = allvarsP p
         xx'       = allvarsX xx
         ns''      = Set.unions [ns', np', annX xx']
     in  ((p',xx') : ps', ns'')


allvarsP :: Eq n => Pattern n -> (Pattern n, Set.Set (Name n))
allvarsP
 = goPat
 where
   goPat p =
    case p of
     PatCon c ps
       -> let (ps',ns') = goPats ps
         in  (PatCon c ps', ns')
     PatDefault
       -> (PatDefault, Set.empty)
     PatVariable n
       -> (PatVariable n,  Set.singleton n)
     PatLit l n
       -> (PatLit l n, Set.empty)

   goPats []
     = ([], Set.empty)
   goPats (p:ps)
     = let
         (p',n')   = goPat p
         (ps',ns') = goPats ps
       in
         (p' : ps', n' `Set.union` ns')
