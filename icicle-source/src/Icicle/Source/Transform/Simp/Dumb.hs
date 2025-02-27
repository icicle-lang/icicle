{-# LANGUAGE NoImplicitPrelude #-}

-- | * Simplify dumb roundabout bindings.
--     e.g. `let x = y in body` or `case x | y -> body`
--
module Icicle.Source.Transform.Simp.Dumb
  ( dumbSimpTransform
  , simpDumbQT
  , simpDumbQ
  , simpDumbX
  ) where

import           Icicle.Source.Query
import           Icicle.Source.Transform.Base

import           Data.Functor.Identity

import           P


dumbSimpTransform
  :: Eq n
  => Transform Identity () a n
dumbSimpTransform
 = idTransform { transformExp = tranx }

 where
  tranx _ x
   = return ((), simpDumbX x)


simpDumbQT :: Eq n => QueryTop a n -> QueryTop a n
simpDumbQT qt
 = runIdentity $ simplifyNestedQT <$> transformQT dumbSimpTransform qt

simpDumbQ :: Eq n => Query a n -> Query a n
simpDumbQ qq
 = runIdentity $ transformQ dumbSimpTransform qq

simpDumbX :: Eq n => Exp a n -> Exp a n
simpDumbX = simpDumbCases . simpDumbLets

-- | Simplify cases with a single default/variable pattern.
--
simpDumbCases
 :: Exp a n -> Exp a n
simpDumbCases xx
 = case xx of
    Case _ _ [(PatDefault, x)]
     -> simpX x

    Case a e [(PatVariable n, x)]
     -> let q = Query [Let a (PatVariable n) (simpX e)] (simpX x)
        in  Nested a q

    Case a e ps
     -> Case a (simpX e) (fmap (fmap simpX) ps)

    Nested a q
     -> Nested a (simpQ q)

    App a x y
     -> App a (simpX x) (simpX y)

    If a s t f
     -> If a (simpX s) (simpX t) (simpX f)

    Lam a n x
     -> Lam a n (simpX x)

    Access a x f
      -> Access a (simpX x) f

    Record a fs ->
      Record a (simpX <$$> fs)

    Var{}  -> xx
    Prim{} -> xx

 where
  simpX
   = simpDumbCases

  simpQ qq
   = qq { contexts = fmap simpC (contexts qq)
        , final    = simpX (final qq) }

  simpC cc
   = case cc of
      GroupBy a x
       -> GroupBy a (simpX x)
      Distinct a x
       -> Distinct a (simpX x)
      Filter a x
       -> Filter a (simpX x)
      FilterLet a n x
       -> FilterLet a n (simpX x)
      LetFold a (Fold b i w t)
       -> LetFold a (Fold b (simpX i) (simpX w) t)
      Let a n x
       -> Let a n (simpX x)
      LetScan a n x
       -> LetScan a n (simpX x)
      ArrayFold a n1 x
       -> ArrayFold a n1 (simpX x)
      GroupFold a n1 n2 x
       -> GroupFold a n1 n2 (simpX x)
      Windowed{} ->
        cc
      Latest{}   ->
        cc

-- | Simplify nested bindings from variables to variables, e.g. `let x = y ~>..`
--   This recurses into nested queries so it's not a traditional beta-reduction.
--
simpDumbLets :: (Eq n) => Exp a n -> Exp a n
simpDumbLets xx
 = case xx of
    Nested _ q
     -> go q

    Case a e ps
     -> Case a e $ fmap (fmap simpDumbLets) ps

    _ -> xx

 where
  go qq
   = case qq of
       Query [] bd
         -> simpDumbLets bd
       Query (Let _ (PatVariable x) (Var _ y) : cs) bd
         -> go (Query cs (substX x y bd))
       _ -> xx

  substX x y bd
   = case bd of
      Var a n
        | n == x
        -> Var a y
      Nested a q
        -> Nested a $ substQ x y q
      App a e1 e2
        -> App a (substX x y e1) (substX x y e2)
      If a scr true false
        -> If a (substX x y scr) (substX x y true) (substX x y false)
      Case a e pats
        -> Case a (substX x y e) (fmap (substA x y) pats)
      Lam {}
        -> bd
      Var {}
        -> bd
      Prim {}
        -> bd
      Access a e f
        -> Access a (substX x y e) f
      Record a fs
        -> Record a (substX x y <$$> fs)

  substA x y (pat, e)
   | x `elem` varsIn pat = (pat, e)
   | otherwise           = (pat, substX x y e)

  varsIn (PatCon _ as)   = concatMap varsIn as
  varsIn (PatRecord rs)  = concatMap (varsIn . snd) rs
  varsIn (PatVariable v) = [ v ]
  varsIn (PatLit _ _)      = []
  varsIn (PatDefault)    = []

  substC _ _ []
   = (True, [])
  substC x y (cc:rest)
   = let (f, rest') = substC x y rest
     in  case cc of
       Let a pat e
        | x `elem` varsIn pat
        -> (False, Let a pat (substX x y e) : rest)
        | otherwise
        -> (f, Let a pat (substX x y e) : rest')
       LetScan a pat e
        | x `elem` varsIn pat
        -> (False, LetScan a pat (substX x y e) : rest)
        | otherwise
        -> (f, LetScan a pat (substX x y e) : rest')
       LetFold a (Fold pat init work ty)
        | x `elem` varsIn pat
        -> (f, LetFold a (Fold pat (substX x y init) (substX x y work) ty):rest')
        | otherwise
        -> (False, LetFold a (Fold pat (substX x y init) work ty) : rest)
       GroupBy a e
        -> (f, GroupBy a (substX x y e) : rest')
       Distinct a e
        -> (f, Distinct a (substX x y e) : rest')
       Filter a e
        -> (f, Filter a (substX x y e) : rest')
       FilterLet a n e
        -> (f, FilterLet a n (substX x y e) : rest')
       ArrayFold a n1 e
        -> (f, ArrayFold a n1 (substX x y e) : rest')
       GroupFold a n1 n2 e
        -> (f, GroupFold a n1 n2 (substX x y e) : rest')

       Windowed {} -> (f, cc : rest')
       Latest {}   -> (f, cc : rest')

  substQ x y qq
   = let (f, ctxs) = substC x y (contexts qq)
      in qq { contexts = ctxs
            , final    = if f then substX x y (final qq) else final qq }
