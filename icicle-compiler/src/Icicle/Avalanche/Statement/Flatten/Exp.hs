-- | Turn Core primitives into Flat - removing the folds
-- The input statements must be in A-normal form.
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
module Icicle.Avalanche.Statement.Flatten.Exp (
    flatX, flatXS
  ) where

import              Icicle.Avalanche.Statement.Flatten.Algorithms
import              Icicle.Avalanche.Statement.Flatten.Base

import              Icicle.Avalanche.Statement.Statement
import qualified    Icicle.Avalanche.Prim.Flat      as Flat
import qualified    Icicle.Avalanche.Prim.Compounds as Flat

import qualified    Icicle.Core.Exp.Prim           as Core
import qualified    Icicle.Common.Exp.Prim.Minimal as Min

import              Icicle.Common.Base
import              Icicle.Common.Type
import              Icicle.Common.Exp
import              Icicle.Common.Exp.Simp.Beta
import              Icicle.Common.Fresh

import              Icicle.Internal.Pretty

import              P
import              Control.Monad.Trans.Class

import              Data.Hashable                  (Hashable)
import              Data.String                    (IsString)


flatXS  :: (Pretty n, Hashable n, Eq n, IsString n, Show n, Show a)
        => a
        -> [Exp a n Core.Prim]
        -> [Exp a n Flat.Prim]
        -> ([Exp a n Flat.Prim] -> FlatM a n)
        -> FlatM a n

flatXS _       []     ys stm = stm ys
flatXS a_fresh (x:xs) ys stm
 = flatX a_fresh x
 $ \x'
 -> flatXS a_fresh xs (ys <> [x']) stm


-- | Flatten an expression, wrapping the statement with any lets or loops or other bindings
-- The statement function takes the new expression.
flatX   :: forall a n . (Pretty n, Hashable n, Eq n, IsString n, Show n, Show a)
        => a
        -> Exp a n Core.Prim
        -> (Exp a n Flat.Prim -> FlatM a n)
        -> FlatM a n

flatX a_fresh xx stm
 = convX
 where
  -- Do a bit of simplification.
  -- Betas must be converted to lets even if they are not simple values:
  -- Unapplied lambdas aren't really able to be converted, since
  -- we're lifting some expressions to statements, and there is no statement-level
  -- lambda, only expression-level lambda.
  x' = beta
     $ betaToLets a_fresh xx

  -- Annotation plumbing
  flatX'           = flatX        a_fresh
  Flat.FlatOps{..} = Flat.flatOps a_fresh
  makeApps'        = makeApps a_fresh

  xVar   = XVar   a_fresh
  xPrim  = XPrim  a_fresh
  xValue = XValue a_fresh
  xApp   = XApp   a_fresh

  -- Convert the expression. We need to handle variables here, since they might
  -- refer to already flattened expressions.
  convX
   = case xx of
      XVar a n
        -> stm $ XVar a n
      _ -> convX'

  -- Convert the simplified expression.
  convX'
   = case x' of
      -- If it doesn't do anything interesting, we can just call the statement
      -- with the original expression
      XVar a n
       -> stm $ XVar a n
      XValue a vt bv
       -> stm $ XValue a vt bv

      XApp{}
       -- Primitive applications are where it gets interesting.
       -- See below
       | Just (p,xs) <- takePrimApps x'
       -> flatPrim p xs

       -- What is the function of this application?
       --
       -- It's not a primitive.
       -- It's not a lambda, since we did betaToLets above.
       -- It's not a value, since that wouldn't typecheck
       -- It's not a let-bound variable, since lets can't bind funs & so wouldn't typecheck.
       --
       -- Therefore, this should not happen for a valid program.
       | otherwise
       -> lift $ Left $ FlattenErrorApplicationNonPrimitive x'

      XPrim _ p
       -> flatPrim p []

      -- Unapplied lambda: this should not happen for a well-typed program
      XLam{}
       -> lift $ Left $ FlattenErrorBareLambda x'

      -- Convert expression lets into statement lets
      XLet _ n p q
       -> flatX' p
       $ \p'
       -> Let n p' <$> flatX' q stm


  -- Handle primitive applications.
  -- PrimFolds get turned into loops and whatnot
  flatPrim :: Core.Prim -> [Exp a n Core.Prim] -> FlatM a n
  flatPrim p xs
   = case p of
      -- Built-in functions that are supported differently in Source/Core vs. in Avalanche/Sea

      -- User arrays
      Core.PrimMinimal (Min.PrimBuiltinFun (Min.PrimBuiltinArray (Min.PrimBuiltinLength t)))
       -> primApps (Flat.PrimProject $ Flat.PrimProjectArrayLength t) xs []

      Core.PrimMinimal (Min.PrimBuiltinFun (Min.PrimBuiltinArray (Min.PrimBuiltinIndex t)))
       | [array, ix] <- xs
       -> flatX' array
       $ \array'
       -> flatX' ix
       $ \ix'
       -> do accN <- fresh
             stm' <- stm (xVar accN)

             let accT = SumT ErrorT t
             let lenX = xPrim (Flat.PrimProject $ Flat.PrimProjectArrayLength t) `makeApps'` [array']
             let zero = xValue IntT $ VInt 0
             let ge   = xPrim (Flat.PrimMinimal $ Min.PrimRelation Min.PrimRelationGe IntT) `makeApps'` [ix', zero]
             let lt   = xPrim (Flat.PrimMinimal $ Min.PrimRelation Min.PrimRelationLt IntT) `makeApps'` [ix', lenX]
             let if1 a b = If a b mempty

             let unsafeIndex  = xPrim (Flat.PrimUnsafe $ Flat.PrimUnsafeArrayIndex t) `makeApps'` [array', ix']
             let unsafeWrap   = xPrim (Flat.PrimMinimal $ Min.PrimConst $ Min.PrimConstRight ErrorT t) `makeApps'` [unsafeIndex]
             let writeIf      = if1 ge $ if1 lt $ Write accN unsafeWrap

             return $ InitAccumulator
                    (Accumulator accN accT (xValue accT $ VLeft $ VError ExceptIndexOutOfBounds))
                    (writeIf <> Read accN accN accT stm')


      -- Implement heap sort in Avalanche so that it can be melted.
      Core.PrimMinimal (Min.PrimBuiltinFun (Min.PrimBuiltinArray (Min.PrimBuiltinSort t)))
       | [array] <- xs
       -> avalancheHeapSortArray a_fresh stm flatX t array

      -- Arithmetic and simple stuff are easy, just move it over
      Core.PrimMinimal pm
       -> primApps (Flat.PrimMinimal pm) xs []

      -- Handle folds below
      Core.PrimFold pf ta
       -> flatFold pf ta xs


      -- Map: insert value into map, or if key already exists,
      -- apply update function to existing value
      Core.PrimMap (Core.PrimMapInsertOrUpdate tk tv)
       | [upd, ins, key, map]   <- xs
       -> avalancheMapInsertUpdate a_fresh stm flatX tk tv upd ins key map
       -- Map with wrong arguments
       | otherwise
       -> lift $ Left $ FlattenErrorPrimBadArgs p xs

      -- Map : delete a value
      Core.PrimMap (Core.PrimMapDelete tk tv)
       | [key, map]   <- xs
       -> avalancheMapDelete a_fresh stm flatX tk tv key map
       -- Map with wrong arguments
       | otherwise
       -> lift $ Left $ FlattenErrorPrimBadArgs p xs

      -- Map: lookup by key
      Core.PrimMap (Core.PrimMapLookup tk tv)
       | [map, key]   <- xs
       -> avalancheMapLookup a_fresh stm flatX tk tv key map
       -- Map with wrong arguments
       | otherwise
       -> lift $ Left $ FlattenErrorPrimBadArgs p xs


      -- Map: create new empty map, for each element, etc
      Core.PrimMap (Core.PrimMapMapValues tk tv tv')
       | [upd, map]   <- xs
       -> flatX' map
       $ \map'
       -> do n'keys <- fresh
             n'vals <- fresh

             stm'   <- flatX'
                     ( xPrim (Core.PrimArray $ Core.PrimArrayMap tv tv')
                       `makeApps'` [upd, xVar n'vals] )
                     (\v -> stm (mapPack tk tv' (xVar n'keys) v))

             return  $ Let n'vals (mapVals tk tv map')
                     $ Let n'keys (mapKeys tk tv map')
                     $ stm'

       -- Map with wrong arguments
       | otherwise
       -> lift $ Left $ FlattenErrorPrimBadArgs p xs


      -- Map over array: create new empty array, for each element, etc
      Core.PrimArray (Core.PrimArrayMap ta tb)
       | [upd, arr]   <- xs
       -> flatX' arr
       $ \arr'
       -> do    accN <- fresh
                stm' <- stm (xVar accN)

                let arrT = ArrayT tb

                loop <- forI'   (arrLen ta arr')                  $ \iter
                     -> fmap    (Read accN accN arrT)             $
                        slet'   (arrIx ta arr' iter)              $ \elm
                     -> flatX'  (upd `xApp` elm)                  $ \elm'
                     -> slet'   (arrUpd tb (xVar accN) iter elm') $ \arr''
                     -> return  (Write accN arr'')

                return $ InitAccumulator
                            (Accumulator accN arrT (arrNew tb (arrLen ta arr')))
                            (loop <> Read accN accN arrT stm')


       -- Map with wrong arguments
       | otherwise
       -> lift $ Left $ FlattenErrorPrimBadArgs p xs


      -- LatestPush b f v -> (BufPush (fst b) f, BufPush (snd b) v)
      Core.PrimLatest (Core.PrimLatestPush i t)
       | [buf, e]    <- xs
       -> flatX' e
       $  \e'
       -> flatX' buf
       $  \buf'
       -> do   tmpN      <- fresh
               let pushed = bufPush i t buf' e'
               stm'      <- stm (xVar tmpN)
               return $ Let tmpN pushed stm'

       | otherwise
       -> lift $ Left $ FlattenErrorPrimBadArgs p xs


      -- LatestRead b     -> BufRead (snd b)
      Core.PrimLatest (Core.PrimLatestRead i t)
       | [buf] <- xs
       -> flatX' buf
       $  \buf'
       -> do   tmpN       <- fresh
               let fpRead  = bufRead i t buf'
               stm'       <- stm (xVar tmpN)
               return $ Let tmpN fpRead stm'

       | otherwise
       -> lift $ Left $ FlattenErrorPrimBadArgs p xs

  -- Convert arguments to a simple primitive.
  -- conv is what we've already converted
  primApps p [] conv
   = stm
   $ makeApps' (xPrim p)
   $ reverse conv

  primApps p (a:as) conv
   = flatX' a
   $ \a'
   -> primApps p as (a' : conv)

  -- Handle primitive folds
  --
  -- Bool is just an if
  flatFold Core.PrimFoldBool valT [then_, else_, pred]
   = flatX' pred
   $ \pred'
   -> do -- Fresh name for accumulator and result.
         -- We can use same name for acc & result variables because accumulators and variables are in different scopes
         acc <- fresh
         -- Compute the rest of the computation, assuming we've stored result in variable named acc
         stm' <- stm (xVar acc)
         let vunit = xValue UnitT VUnit
         sthen <- flatX' (then_ `xApp` vunit) (return . Write acc)
         selse <- flatX' (else_ `xApp` vunit) (return . Write acc)
         -- Perform if and write result
         let if_ =  If pred' sthen selse
         -- After if, read back result from accumulator and then go do the rest of the statements
         let read_ = Read acc acc valT stm'
         return (InitAccumulator (Accumulator acc valT $ xValue valT $ defaultOfType valT) (if_ <> read_))

  -- Array fold becomes a loop
  flatFold (Core.PrimFoldArray telem) valT [k, z, arr]
   = do accN <- fresh
        stm' <- stm (xVar accN)

        -- Loop body updates accumulator with k function
        loop <-  flatX' arr                             $ \arr'
             ->  forI'   (arrLen telem  arr')           $ \iter
             ->  fmap    (Read accN accN valT)          $
                 slet'   (arrIx  telem arr' iter)       $ \elm
             ->  flatX'  (makeApps' k [xVar accN, elm]) $ \x
             ->  return (Write accN x)

        -- Initialise accumulator with value z, execute loop, read from accumulator
        flatX' z $ \z' ->
            return (InitAccumulator (Accumulator accN valT z')
                   (loop <> Read accN accN valT stm'))


  -- Fold over map. Very similar to above
  flatFold (Core.PrimFoldMap tk tv) valT [k, z, mmm]
   =  flatX' mmm
   $ \mmm'
   -> do n'keys  <- fresh
         n'vals  <- fresh
         n'zips  <- fresh

         n'kv <- fresh
         n'ac <- fresh
         let k'   = XLam a_fresh n'ac  valT
                  $ XLam a_fresh n'kv (PairT tk tv)
                  ( k `makeApps'` [ xVar n'ac
                                  , projCore False tk tv $ xVar n'kv
                                  , projCore True  tk tv $ xVar n'kv ])

         res     <- flatX'
                  ( xPrim (Core.PrimFold (Core.PrimFoldArray (PairT tk tv)) valT)
                     `makeApps'` [k', z, xVar n'zips] ) stm

         return   $ Let n'keys (mapKeys tk tv mmm')
                  $ Let n'vals (mapVals tk tv mmm')
                  $ Let n'zips (arrZip  tk tv (xVar n'keys) (xVar n'vals))
                  $ res



  -- Fold over an option is just "maybe" combinator.
  flatFold (Core.PrimFoldOption ta) valT [xsome, xnone, opt]
   = flatX' opt
   $ \opt'
   -> do
      acc  <- fresh
      stm' <- stm (xVar acc)
      tmp  <- fresh
      let vunit = xValue UnitT VUnit
      ssome <- flatX' (xsome `xApp` (xVar tmp)) $ (return . Write acc)
      snone <- flatX' (xnone `xApp` vunit)  $ (return . Write acc)
      let if_   = If (isSome ta opt') (Let tmp (optionGet ta opt') ssome) snone
          -- After if, read back result from accumulator and then go do the rest of the statements
          read_ = Read acc acc valT stm'
      return (InitAccumulator (Accumulator acc valT $ xValue valT $ defaultOfType valT) (if_ <> read_))

  -- Fold over an either
  flatFold (Core.PrimFoldSum ta tb) valT [xleft, xright, scrut]
   = flatX' scrut
      $ \scrut'
      -> do
         acc  <- fresh
         stm' <- stm (xVar acc)
         tmp  <- fresh
         tmp' <- fresh
         sleft   <- flatX' (xleft  `xApp` xVar tmp ) (return . Write acc)
         sright  <- flatX' (xright `xApp` xVar tmp') (return . Write acc)

         let if_   = If (isRightF ta tb scrut')
                        (Let tmp' (right ta tb scrut') sright)
                        (Let tmp  (left  ta tb scrut') sleft)
           -- After if, read back result from accumulator and then go do the rest of the statements
             read_ = Read acc acc valT stm'
         return (InitAccumulator (Accumulator acc valT $ xValue valT $ defaultOfType valT) (if_ <> read_))


  -- None of the above cases apply, so must be bad arguments
  flatFold pf rt xs
   = lift $ Left $ FlattenErrorPrimBadArgs (Core.PrimFold pf rt) xs

  -- Create a fst# or snd#
  proj pmin t ta tb e
   = let pm | not t
            = Min.PrimPairFst ta tb
            | otherwise
            = Min.PrimPairSnd ta tb
     in (xPrim $ pmin $ Min.PrimPair $ pm) `xApp` e
  projCore = proj Core.PrimMinimal

  slet'      = slet a_fresh
  forI'      = forI a_fresh
