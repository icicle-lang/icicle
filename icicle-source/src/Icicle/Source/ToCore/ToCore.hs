-- | Converting Source to Core.
--
--   This is the main entry point for this conversion, setting up
--   the conversion state and handling top level evaluations.
--
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections     #-}
module Icicle.Source.ToCore.ToCore (
    convertQueryTop
  , convertQuery
  ) where

import                  Icicle.Source.Query
import                  Icicle.Source.ToCore.Base
import                  Icicle.Source.ToCore.Exp
import                  Icicle.Source.ToCore.Fold
import                  Icicle.Source.ToCore.Prim
import                  Icicle.Source.Type

import qualified        Icicle.Core as C
import qualified        Icicle.Core.Exp.Combinators as CE
import qualified        Icicle.Common.Exp           as CE
import qualified        Icicle.Common.Exp.Prim.Minimal as Min
import qualified        Icicle.Common.Exp.Simp.Beta    as Beta
import qualified        Icicle.Common.Type as T
import                  Icicle.Common.Fresh
import                  Icicle.Common.Base

import                  Icicle.Data.Name

import                  P

import                  Control.Monad.Morph
import                  Control.Monad.Trans.State.Lazy

import                  Data.Hashable (Hashable)
import                  Data.List (zip, unzip)
import qualified        Data.Map as Map


-- | Convert a top-level Query to Core.
--
-- The input query is annotated with its type information at every node.
-- This is essential for both putting type annotations on the explicitly typed core,
-- and also because the universe annotations determine where computations are done:
--
-- "Pure" computations become Pre-computations in Core,
-- since they must not use on any stream inputs;
--
-- "Elem" computations must be worker functions on filters, maps, and so on.
--
-- "Aggregate" computations can be reductions on streams, or postcomputations.
--
convertQueryTop :: (Hashable n, Eq n)
                => Features () n (InputKey (Annot a n) n)
                -> QueryTop (Annot a n) n
                -> FreshT n (Either (ConvertError a n)) (C.Program () n)
convertQueryTop feats qt
 = do   inp         <- fresh
        facttime    <- fresh
        now         <- fresh
        maxMapSize  <- fresh

        -- Lookup the fact that this query refers to.
        FeatureConcrete key ty fs
          <- lift
           $ maybeToRight (ConvertErrorNoSuchFeature (queryInput qt))
           $ lookupInputId (queryInput qt) (featuresConcretes feats)

        -- Extract the type of the input stream, pair with the fact time.
        inpTy <- case valTypeOfType ty of
                  Nothing -> lift $ Left $ ConvertErrorCannotConvertType (annAnnot $ annotOfQuery $ query qt) ty
                  Just t' -> return t'
        let inpTy'dated = T.PairT inpTy T.TimeT

        let convState = ConvertState inp inpTy'dated facttime now maxMapSize fs Map.empty Map.empty
        let env       = Map.insert facttime (T.funOfVal   T.TimeT)
                      $ Map.insert inp      (T.funOfVal $ T.PairT inpTy T.TimeT) Map.empty

        -- Convert the query body.
        (bs, ret) <- flip evalStateT convState
                   $ do maybe (return ()) (flip convertFreshenAddAs now) $ featureNow feats
                        convertQuery (query qt) >>= convertKey env key

        return (programOfBinds (queryName qt) inpTy inp facttime now maxMapSize bs () ret)


-- | Convert a Query to Core
--
--   This takes the name of the input stream, the element types of the input,
--   and the query to transform.
--   It returns a list of program bindings, as well as the name of the binding
--   that is being "returned" in the program - essentially the last added binding.
--
--   Results using convertAsFold should always give identical results, but
--   performing some elaboration here allows for better query fusion and sub
--   expression elimination; as well as just making queries easier to read.
--
convertQuery :: (Hashable n, Eq n)
             => Query (Annot a n) n
             -> ConvertM a n (CoreBinds () n, Name n)
convertQuery q
 = convertContext
 $ case contexts q of
    -- There are no queries left, so deal with simple aggregates and nested queries.
    []
     -> convertReduce (final q)

    -- Converting filters is probably the simplest conversion.
    --
    -- We convert the query as per usual, but then repack its
    -- streams into a Core stream filter.
    (Filter _ e : _)
     -> do  e'      <- convertExp e
            (bs, b) <- convertQuery q'
            let bs' = filt e' (streams bs) <> bs { streams = [] }
            return (bs', b)

    -- Windowing in Core is a standard filter, with pre-computations for
    -- the edges of the windows.
    (Windowed _ newerThan olderThan : _)
     -> do  (bs, b)   <- convertQuery q'
            now       <- convertDateName
            time      <- convertFactTimeName
            leftEdge  <- lift fresh
            rightEdge <- lift fresh

            -- Bind the computations of the window
            -- edges as a pre-computations.
            let
              -- All widows have a left edge
              leftPrecomp
                = pre leftEdge $ windowEdge (CE.xVar now) newerThan
              leftComparison
                = CE.xVar time >=~ CE.xVar leftEdge

              -- If olderThan is set, we need to create
              -- the right edge of the window as well;
              -- otherwise, leave the comparison and
              -- precomputations as just the left edge.
              (bothPre, bothComparison)
                = case olderThan of
                    Just olderThan' ->
                      let
                        rightPrecomp =
                          pre rightEdge $ windowEdge (CE.xVar now) olderThan'
                        rightComparison =
                          CE.xVar rightEdge >=~ CE.xVar time
                      in
                        (leftPrecomp <> rightPrecomp, leftComparison CE.&&~ rightComparison)

                    Nothing ->
                      (leftPrecomp, leftComparison)

              -- Create the filtered program by filtering the downstream
              -- streams. We leave the precomputations and postcomputations
              -- of `bs` alone, and combine them with the new program.
              filteredProgram =
                filt bothComparison (streams bs) <> bs { streams = [] }

            return (bothPre <> filteredProgram, b)
          where
            (>=~) :: C.Exp () n -> C.Exp () n -> C.Exp () n
            (>=~) = CE.prim2 (C.PrimMinimal $ Min.PrimRelation Min.PrimRelationGe T.TimeT)
            infix 4 >=~

    (FilterLet {} : _)
     -> convertAsFold

    (Latest {} : _)
     -> convertAsFold

    (GroupBy {} : _)
     -> convertAsFold

    (ArrayFold {} : _)
     -> convertAsFold

    (Distinct {} : _)
     -> convertAsFold

    -- Convert a group fold using a Map. Very similar to Group By, with an additional
    -- postcomputation.
    --
    -- The group itself constructs the Map and the group fold perform its aggregate
    -- on the Map.
    --
    (GroupFold (Annot { annAnnot = ann }) (PatVariable k) (PatVariable v) e : _ )
     -> do  (tk, tv) <- getGroupFoldType ann e

            n'   <- lift fresh
            nacc <- lift fresh

            -- Convert the inner group into a stream fold that produces a map.
            (bs, nm) <- convertReduce e

            -- The key and value will be available after the fold
            k' <- convertFreshenAdd k
            v' <- convertFreshenAdd v

            -- Convert the rest of the query into a map fold.
            res      <- convertFold q'
            let tacc  = typeFold res

            -- Perform the map fold.
            let p = post n'
                  $ beta
                  ( mapExtract res CE.@~
                  ( CE.xPrim
                      (C.PrimFold (C.PrimFoldMap tk tv) tacc)
                    CE.@~ ( CE.xLam nacc tacc
                          $ CE.xLam k'   tk
                          $ CE.xLam v'   tv
                              (foldKons res CE.@~ CE.xVar nacc))
                    CE.@~ foldZero res
                    CE.@~ CE.xVar nm))

            return (bs <> p, n')

    -- Group folds with patterns should have been desugared
    (GroupFold (Annot { annAnnot = ann }) (PatVariable _) pat _ : _)
     -> convertError $ ConvertErrorPatternUnconvertable ann pat

    (GroupFold (Annot { annAnnot = ann }) pat _ _ : _)
     -> convertError $ ConvertErrorPatternUnconvertable ann pat

    -- Let Scans introduce two Core folds before compiling the
    -- rest of the query.
    --
    -- The first is the normal folding aggregation for the bound
    -- subquery; the second contains the final extraction step,
    -- running at every query iteration.
    (LetScan _ (PatVariable b) def : _)
     -> do res        <- convertFold $ Query [] def
           n'red      <- lift fresh

           let k'      = beta
                       ( foldKons res CE.@~ CE.xVar n'red )

           let y'      = beta
                       ( mapExtract res CE.@~ CE.xVar n'red )

           let b'red   = sfold n'red (typeFold res) (foldZero res) k'

           let retTy   = typeExtract res
           let def'y   = CE.xValue retTy (T.defaultOfType retTy)
           b'         <- convertFreshenAdd b
           let b'ret   = sfold b' retTy def'y y'

           -- Inform that this needs to be packed into buffers
           -- if we run a 'latest'.
           (bs',n'')  <- convertContext $ do
             convertAddElementRepack b b' retTy
             convertQuery q'

           return (b'red <> b'ret <> bs', n'')


    (Let _ (PatVariable b) def : _)
     -> case getTemporalityOrPure $ annResult $ annotOfExp def of
         TemporalityElement
          -> do t'         <- convertValType' $ annResult $ annotOfExp def
                e'         <- convertExp def
                let def't   = CE.xValue t' (T.defaultOfType t')
                b'         <- convertFreshenAdd b
                let bs      = sfold b' t' def't e'

                -- Inform that this needs to be packed into buffers
                -- if we run a 'latest'.
                (bs', n'') <- convertContext $ do
                  convertAddElementRepack b b' t'
                  convertQuery q'

                return (bs <> bs', n'')

         TemporalityPure
          -> do e'        <- convertExp def
                b'        <- convertFreshenAdd b
                let bs     = pre b' e'
                (bs', n') <- convertQuery q'
                return (bs <> bs', n')

         TemporalityAggregate
          -> do (bs,n')      <- convertReduce def
                b'           <- convertFreshenAdd b
                (bs',n'')    <- convertQuery q'
                return (bs <> post b' (CE.xVar n') <> bs', n'')

         _
          -> convertError $ ConvertErrorGroupByHasNonGroupResult (annAnnot $ annotOfExp def) (annResult $ annotOfExp def)


    (LetScan (Annot { annAnnot = ann }) pat _ : _)
     -> convertError $ ConvertErrorPatternUnconvertable ann pat

    (Let (Annot { annAnnot = ann }) pat _ : _)
     -> convertError $ ConvertErrorPatternUnconvertable ann pat

    -- In comparison, normal folds are quite easy.
    --
    -- > let fold summ ~> 0 : summ + value ~> ...
    -- >
    -- > =====>
    -- >
    -- > Stream.fold
    -- > (\a : Int. \v : Value. a + v)
    -- > 0
    -- > stream
    --
    (LetFold _ f@Fold{ foldType = FoldTypeFoldl, foldBind = PatVariable n } : _)
     -> do  -- Type helpers
            tU <- convertValType' $ annResult $ annotOfExp $ foldWork f

            z   <- convertExp (foldInit f)
            -- Current accumulator is only available in worker
            -- Remove binding before converting init and work expressions,
            -- just in case the same name has been used elsewhere
            n'a <- convertFreshenAdd n
            k   <- convertExp (foldWork f)

            -- Bind the fold to the original name
            let bs = sfold n'a tU z k

            (bs', n'')      <- convertQuery q'

            return (bs <> bs', n'')

    -- Converting fold1s.
    (LetFold (Annot { annAnnot = ann }) Fold{ foldType = FoldTypeFoldl1 } : _)
     -> convertError $ ConvertErrorImpossibleFold1 ann

    -- Converting folds with patterns.
    (LetFold (Annot { annAnnot = ann }) Fold{ foldBind = pat } : _)
     -> convertError $ ConvertErrorPatternUnconvertable ann pat

 where
  -- The remaining query after the current context is removed
  q' = q { contexts = drop 1 $ contexts q }

  -- Perform beta reduction, just to simplify the output a tiny bit.
  beta = Beta.betaToLets ()
       . Beta.beta

  convertValType' = convertValType (annAnnot $ annotOfQuery q)

  getGroupFoldType  = groupFoldType convertValType'

  -- When the convertFold and convertQuery are exactly the same, just use convertFold instead.
  convertAsFold = do
    res        <- convertFold q
    n'red      <- lift fresh
    n'ret      <- lift fresh

    let k'      = beta
                ( foldKons res CE.@~ CE.xVar n'red)

    let b'red   = sfold n'red (typeFold res) (foldZero res) k'
    let b'ret   = post n'ret $ beta (mapExtract res CE.@~ CE.xVar n'red)

    return (b'red <> b'ret, n'ret)


-- | Convert an Aggregate computation at the end of a query.
-- This should be an aggregate, some primitive applied to at least one aggregate expression,
-- or a nested query.
-- If not, it must be pure, so we can just bind it as a precomputation.
--
convertReduce :: (Hashable n, Eq n)
              => Exp (Annot a n) n
              -> ConvertM a n (CoreBinds () n, Name n)
convertReduce xx
 -- If it is Pure, just bind it as a precomputation
 | TemporalityPure <- getTemporalityOrPure $ annResult $ annotOfExp xx
 = do   x' <- convertExp xx
        nm <- lift fresh
        return (pre nm x', nm)

 -- For primitives:
 --   Recurse into their arguments and get bindings for them,
 --   Apply those bindings to the primitive as a postcomputation.
 | Just (p, Annot { annResult = ty }, args) <- takePrimApps xx
 = do   (bs,nms) <- unzip <$> mapM convertReduce args
        let tys   = fmap (annResult . annotOfExp) args
        let xs    = fmap  CE.xVar           nms
        x'       <- convertPrim p (annAnnot $ annotOfExp xx) ty (xs `zip` tys)
        nm       <- lift fresh
        return (mconcat bs <> post nm x', nm)

 -- Convert a nested query
 -- Any lets, folds etc bound in here will go out of scope at the end.
 -- So we revert the state at the end, clearing any bindings,
 -- rolling back to the old input type, etc.
 | Nested _ q   <- xx
 = do   o <- get
        r <- convertQuery q
        put o
        return r

 -- Any variable must be a let-bound aggregate, so we can safely assume it has a binding.
 | Var (Annot { annAnnot = ann }) v      <- xx
 = (,) mempty <$> convertFreshenLookup ann v


 | If (Annot { annAnnot = ann, annResult = retty }) scrut true false <- xx
 = do   scrut' <- convertReduce scrut

        -- Because the alternatives can contain more folds and stuff, we need to
        -- use convertReduce on them.
        true'  <- convertReduce true
        false' <- convertReduce false

        -- The folds, pre- and post- computations for all the components
        let bs' = fst scrut' <> fst true' <> fst false'

        resT   <- convertValType ann $ retty

        -- The core fold expression over the resulting name from the scrutinee,
        -- applied to the names of the true and false branches.
        sn     <- lift fresh
        let x'  = (CE.xPrim $ C.PrimFold C.PrimFoldBool resT)
                    CE.@~ CE.xLam sn T.UnitT (CE.xVar $ snd true')
                    CE.@~ CE.xLam sn T.UnitT (CE.xVar $ snd false')
                    CE.@~ (CE.xVar $ snd scrut')

        nm     <- lift fresh

        return (bs' <> post nm x', nm)

 | Case (Annot { annAnnot = ann, annResult = retty }) scrut patalts <- xx
 = do   scrut' <- convertReduce scrut

        -- Because the alternatives can contain more folds and stuff, we need to
        -- use convertReduce on them.
        -- However, because the pattern variables must be Aggregates, we know that
        -- any pattern variables will only be mentioned in the postcomputations.
        -- Therefore we need to pull out the postcomputations into a let,
        -- and stick them inside the new case alternative.
        --
        -- All the foldy bits of each alternative must be computed, because
        -- we won't know which ones will be needed until after they are run.
        let goPatAlt (p,alt)
                  = (,) <$> convertCaseFreshenPat p <*> convertReduce alt
        patalts' <- mapM goPatAlt patalts
        let pats' = fmap                 fst  patalts'
            alts' = fmap (pullPosts ())       patalts'

        let bs' = fst scrut' <> mconcat (fmap fst alts')

        let sX  = CE.xVar $ snd scrut'
        let aXs = fmap snd alts'

        scrutT <- convertValType ann $ annResult $ annotOfExp scrut
        resT   <- convertValType ann $ retty

        x'     <- convertCase xx sX (pats' `zip` aXs) scrutT resT
        nm     <- lift fresh

        return (bs' <> post nm x', nm)

 | Access a@(Annot { annAnnot = ann }) accessed field <- xx
 = do   -- Convert the accessed expression
        res    <- convertReduce accessed
        accT   <- convertValType ann $ annResult $ annotOfExp accessed

        let bs  = fst res
        let acc = CE.xVar $ snd res

        -- Map the final result:
        --  convert the access of the reduced bound variable.
        access <- convertAccess a acc accT field
        nm     <- lift fresh

        return (bs <> post nm access, nm)


 | Record (Annot { annAnnot = ann, annResult = retty }) fields <- xx
 = do   fields'    <- Map.fromList <$> traverse (traverse convertReduce) fields
        fieldsT    <- convertValType ann retty

        let bs      = foldMap fst $ Map.elems fields'
        let acc     = CE.xVar . snd <$> Map.elems fields'

        structT    <- case fieldsT of
                        T.StructT st ->
                          return st
                        _ ->
                          convertError
                            $ ConvertErrorInputTypeNotMap ann fieldsT

        let fin = CE.makeApps () (CE.xPrim $ C.PrimMinimal $ Min.PrimStruct $ Min.PrimStructConstruct structT) acc
        nm         <- lift fresh

        return (bs <> post nm fin, nm)

 -- It's not a variable or a nested query,
 -- so it must be an application of a non-primitive
 | otherwise
 = convertError $ ConvertErrorExpApplicationOfNonPrimitive (annAnnot $ annotOfExp xx) xx

-- | Incorporate the refutation fact key into the query.
--
-- @
--   INIT:
--     (none, <fold_init>)
--   KONS:
--     new_key = nub_exp value
--     fold_bool
--       (old_key, acc)
--       (new_key, <fold_kons>)
--       (some new_key == old_key)
-- @
--
convertKey :: (Hashable n, Eq n)
           => T.Env n T.Type
           -> InputKey (Annot a n) n
           -> (CoreBinds () n, Name n)
           -> ConvertM a n (CoreBinds () n, Name n)
convertKey _   (InputKey Nothing)  bs          = return bs
convertKey env (InputKey (Just k)) (core, ret) = do
  -- Convert the key expression to Core.
  k'        <- convertExp k

  -- Synthesise a type for the key expression.
  t'k       <- lift . lift
             . bimap  (ConvertErrorCannotCheckKey (annAnnot (annotOfExp k)) k')
                      T.functionReturns
             $ CE.typeExp C.coreFragmentWorkerFun env k'
  let t'key  = T.OptionT t'k

  let pairX t u = CE.xPrim . C.PrimMinimal . Min.PrimConst    $ Min.PrimConstPair  t u
  let fstX  t u = CE.xPrim . C.PrimMinimal . Min.PrimPair     $ Min.PrimPairFst    t u
  let sndX  t u = CE.xPrim . C.PrimMinimal . Min.PrimPair     $ Min.PrimPairSnd    t u
  let someX t   = CE.xPrim . C.PrimMinimal . Min.PrimConst    $ Min.PrimConstSome  t
  let eqX   t   = CE.xPrim . C.PrimMinimal $ Min.PrimRelation   Min.PrimRelationEq t

  let nubKons t'acc n'input xx = do
        n'acc'old <- lift fresh
        n'acc'new <- lift fresh
        n'key'old <- lift fresh
        n'key'new <- lift fresh
        n'unit    <- lift fresh

        xx' <- lift $ CE.subst1 () n'input (CE.xVar n'acc'old) xx
        let a  = CE.makeApps () (sndX  t'key t'acc) [CE.xVar n'input]
        let x' = CE.xLet n'key'old (CE.makeApps () (fstX  t'key t'acc) [CE.xVar n'input])
               $ CE.xLet n'acc'old a
               $ CE.xLet n'key'new (CE.makeApps () (someX t'k        ) [k'             ])
               $ CE.xLet n'acc'new xx'
               $ CE.makeApps ()
                   ( CE.xPrim (C.PrimFold C.PrimFoldBool (T.PairT t'key t'acc)) )
                   [ CE.xLam n'unit T.UnitT $ CE.xVar n'input
                   , CE.xLam n'unit T.UnitT $ CE.makeApps () (pairX t'key t'acc) [ CE.xVar n'key'new, CE.xVar n'acc'new ]
                   , CE.makeApps () (eqX   t'key      ) [ CE.xVar n'key'old, CE.xVar n'key'new ] ]

        pure ( x', Map.singleton n'input a )

  let nubInit t'acc xx
        = CE.makeApps () (pairX t'key t'acc) [ CE.xValue t'key VNone, xx ]

  -- Substitute the name of the stream input in the body of this stream and all downstreams,
  -- since the stream input should now refer to the key paired with the original input.
  let substStream subs (C.SFold f t ini kons)
        =   C.SFold f t
        <$> CE.subst () subs ini
        <*> CE.subst () subs kons
      substStream subs (C.SFilter f ss)
        =   C.SFilter
        <$> CE.subst () subs f
        <*> mapM (substStream subs) ss

  let substThen f (s : ss) = do
        (s', substs)   <- f [s]
        (ss', substs') <- f =<< lift (mapM (substStream substs) ss)
        pure (s' <> ss', substs <> substs')
      substThen _ []
        = pure ([], Map.empty)

  -- Prefix each streams with the nub expression.
  let nub (C.SFold n t ini kons : downstreams) = do
        (x', sub)    <- nubKons t n kons
        downstreams' <- lift $ mapM (substStream sub) downstreams
        (ss, subs)   <- nub downstreams'
        let s = C.SFold n (T.PairT t'key t) (nubInit t ini) x'
        pure (s : ss, sub <> subs)

      nub (C.SFilter x fs : downstreams) = do
        (fs', sub)    <- substThen nub fs
        downstreams'  <- lift $ mapM (substStream sub) downstreams
        (ss, subs)    <- nub downstreams'
        let s = C.SFilter x fs'
        pure (s : ss, sub <> subs)

      nub [] = pure ([], Map.empty)

  -- Remove the key at the end.
  let unkey ps [C.SFold n t'ret _ _]
        | n == ret = do
            n'ret <- lift fresh
            pure (ps <> [(n'ret, CE.makeApps () (sndX t'key t'ret) [CE.xVar n])], n'ret)
        | otherwise = pure (ps, ret)
      unkey p [C.SFilter _ ss] = unkey p ss
      unkey p (_ : ss)         = unkey p ss
      unkey p []               = pure (p, ret)

  (streams', subs)    <- hoist runFreshIdentity . nub
                       . streams $ core
  postcomps'          <- lift . runFreshIdentity
                       . mapM (\(n, x) -> (n,) <$> CE.subst () subs x)
                       . postcomps $ core
  (postcomps'', ret') <- unkey postcomps' (streams core)

  return ( core { streams = streams'
                , postcomps = postcomps'' }
         , ret' )
