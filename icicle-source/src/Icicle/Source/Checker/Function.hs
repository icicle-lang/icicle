-- | Typecheck and generalise functions
{-# LANGUAGE DoAndIfThenElse   #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE       QuasiQuotes #-}
{-# LANGUAGE RankNTypes        #-}
module Icicle.Source.Checker.Function (
    checkF
  , checkModule

  , checkF'
  ) where

import                  Icicle.Source.Checker.Base
import                  Icicle.Source.Checker.Checker
import                  Icicle.Source.Checker.Error
import                  Icicle.Source.Checker.Constraint
import                  Icicle.Source.Checker.Subsumption

import                  Icicle.Source.Query
import                  Icicle.Source.Type
import                  Icicle.Source.Lexer.Token          (Variable (..))

import                  Icicle.Common.Base      (Name)
import qualified        Icicle.Common.Fresh     as Fresh

import                  Icicle.Internal.Pretty (Pretty, pretty)

import                  P

import                  Control.Monad.Trans.Either
import                  Control.Monad.Trans.Class (lift)

import qualified        Data.Map                as Map
import qualified        Data.Set                as Set
import                  Data.Hashable           (Hashable)


type FunEnvT a = Features () Variable (InputKey (Annot a Variable) Variable)


checkModule
  :: CheckOptions
  -> FunEnvT a
  -> Module a Variable
  -> EitherT (CheckError a Variable, [CheckLog a Variable]) (Fresh.Fresh Variable)
             (ResolvedModule a Variable, [[CheckLog a Variable]])


checkModule checkOpts env module'
 = do (entries, inputs, outputs, logs) <- foldlM go ([],Map.empty,Map.empty,[]) (moduleEntries module')
      return $
        (ResolvedModule (moduleName module') (moduleImports module') inputs outputs entries, logs)

 where
  go (env0,inputs0,output0,logs0) (DeclInput ann iid inputVT (InputKey iKey)) = do
    let
      FeatureConcrete _ _ plainContext =
        mkFeatureContext inputVT unkeyed
      keyEnv =
        fmap function0 (envOfFeatureContext plainContext)

    checkedKey <- firstEitherT (,[]) $ traverse (checkKey keyEnv) iKey
    pure (env0, Map.insert iid (ModuleInput ann iid inputVT (InputKey checkedKey)) inputs0, output0, logs0)

  go (env0,inputs0,output0,logs0) (DeclOutput _ oid outputQT) = do
    let
      features =
        featureMapOfAccumulator env env0 inputs0

    (checked,_) <- firstEitherT (,[]) $ checkQT checkOpts features outputQT

    pure (env0, inputs0, Map.insert oid checked output0, logs0)

  go (env0,inputs0,output0,logs0) (DeclFun ann name t fun) = do
    let
      envMap =
        Map.union
          (Map.fromList $ fmap ((,) <$> functionName <*> functionType) env0)
          (featuresFunctions env)

    (checkResult,logs') <-
      lift $ checkF envMap ann t fun

    (annotfun, funtype) <-
      hoistEither $ first (,logs') checkResult

    if Map.member name envMap then
      hoistEither $ Left (CheckError (ErrorDuplicateFunctionNames ann name) [], [])
    else
      pure (env0 <> [ResolvedFunction ann name funtype annotfun], inputs0, output0, logs0 <> [logs'])


checkF
  :: (Hashable n, Eq n, Pretty n)
  => Map.Map (Name n) (Scheme n)
  -> a
  -> Maybe (Scheme n)
  -> Exp a n
  -> (Fresh.Fresh n) (Either (CheckError a n) (Exp (Annot a n) n, Scheme n), [CheckLog a n])

checkF env a t fun
 = evalGen $ checkF' fun env >>= constrain a t


-- | Get all the features and facts from a dictionary.
--
featureMapOfAccumulator
  :: Foldable t
  => FunEnvT a
  -> [ResolvedFunction a Variable]
  -> t (ModuleInput a Variable)
  -> Features () Variable (InputKey (Annot a Variable) Variable)
featureMapOfAccumulator features env0 inputs0
 = features
     { featuresFunctions = Map.union moreF (featuresFunctions features)
     , featuresConcretes = Map.union moreC (featuresConcretes features)
     }
 where
  mkFeatureContextX (ModuleInput _ iid enc k) =
    (iid, mkFeatureContext enc k)
  moreC =
    Map.fromList $ fmap mkFeatureContextX (toList inputs0)
  moreF =
    Map.fromList $ fmap ((,) <$> functionName <*> functionType) env0

constrain :: (Hashable n, Eq n, Pretty n)
          => a
          -> Maybe (Scheme n)
          -> (Exp (Annot a n) n, Scheme n)
          -> Gen a n (Exp (Annot a n) n, Scheme n)
constrain ann sig (x, inferred) = do
  case sig of
    -- No explicit type signature
    -- Return the inferred result.
    Nothing ->
      return (x, inferred)

    -- We have a type, run subsumption,
    -- seeing if the inferred type is a least
    -- as polymorphic as the explicit type
    -- signature.
    Just explicit -> do
      subsume ann x inferred explicit

-- | Typecheck a function definition, generalising types and pulling out constraints
checkF' :: (Hashable n, Eq n, Pretty n)
        => Exp a n
        -> GenEnv n
        -> Gen a n (Exp (Annot a n) n, Scheme n)

checkF' fun env
 = do let (arguments, body) = takeLams fun
      -- Give each argument a fresh type variable
      env' <- foldM bindArg env arguments
      -- Get the type annotated body
      (q', subs', cons') <- generateX body env'

      -- Perform top-level discharge of any silly
      -- leftover Possibility or Temporality joins
      -- This will reduce the complexity of the our
      -- prelude and user defined function types
      -- considerably.
      (q, subs, cons) <- dischargeF q' subs' cons'

      -- Look up the argument types after solving all constraints.
      -- Because they started as fresh unification variables,
      -- they will end up being unified to the actual types.
      args <- traverse (lookupArg subs env') arguments

      -- Helper function for the fold below.
      -- Builds a single lambda from an argument to the result;
      -- extracting out the type from the result for the arrow.
      let mkLam (ann, arg) fin =
            let
              ann' = mapSourceType (\argT -> TypeArrow argT (annResult (annotOfExp fin))) ann
            in
              Lam ann' arg fin

      -- The type of the function before we perform mode removal
      -- below.
      let funInferred
            = foldr mkLam q args

      -- Find all leftover constraints
      let constrs = fmap snd cons

      -- We want to remove any modes (temporalities or possibilities)
      -- that are bound by foralls with no constraints on them.
      -- A (forall t : Temporality. t a) is morally equivalent to
      -- an unadorned, or pure, a.
      --
      -- However, removing these 'free modes' has a few small advantages:
      -- - Aesthetically the type "Int" is nicer than "forall t. t Int"
      -- - We don't need to perform let-generalisation on modes in the rest of the typechecker
      --
      -- To illustrate the second point, consider:
      -- > let x = ( 1 + 2          : forall t a. Num a => t a )
      -- > let useInt = x * (value  : Element Int)
      -- > let useDbl = x * (0.5    : Float)
      --
      -- Here, "x" is pure and should be able to be used in both Element and pure contexts.
      -- So "x"'s type must be generalised to "forall t. Num a => t a"
      -- However the "a" must not be generalised into a forall, because that would
      -- cause duplicate computation, and allow x to be used as both an Int and a Float.
      --
      -- This certainly isn't a massive problem.
      -- However, given that we don't have lambdas as expressions, we can get away
      -- with having no let generalisation at all.
      -- So this seems like the simpler option.
      --
      -- XXX mode removal for "unsafeCoerceMode : forall t1 t2. t1 a -> t2 a"
      -- Note that the current implementation is subtly incorrect though, in the presence
      -- of a function of type unsafeCoerceMode.
      -- If you simply removed all the modes for
      -- > forall t1 t2. t1 a -> t2 a
      -- you would get the result
      -- > a -> a
      -- which is quite different!
      --
      -- So this is interesting, but I do not foresee this being an issue in practice.

      -- Get all the names mentioned in constraints.
      -- Any modes mentioned in constraints are not necessarily pure.
      let keepModes
           = Set.unions
           $ fmap freeC constrs

      -- Elide any modes - temporality or possibility - that are not mentioned in constraints
      let remode t
           | Just (TypeVar n) <- t
           , not $ Set.member n keepModes
           = Nothing
           | otherwise
           = t

     -- Take apart temporality and possibility, remode, then put it back together
      let fixModes t
           = let (tmp,pos,dat) = decomposeT t
             in  recomposeT (remode tmp, remode pos, dat)

      -- Our function with skippable modes elided.
      let funRemode
            = reannot (mapSourceType fixModes) funInferred

      -- Type of our final function.
      let funRemodeT
            = annResult $ annotOfExp funRemode

      -- Find free variables in types and constraints - these have to be bound as foralls.
      let binds
            = Set.toList
            $ Set.union keepModes
            $ freeT funRemodeT

      -- Build the type Scheme
      let funT  = Forall binds constrs funRemodeT

      return (funRemode, funT)
 where
  bindArg cx (_,n)
   = do t <- freshType
        return (bindT n t cx)

  freshType
   =    Temporality <$> (TypeVar <$> fresh)
   <*> (Possibility <$> (TypeVar <$> fresh)
   <*>                  (TypeVar <$> fresh))

  lookupArg subs e (a,n)
   = do (_,t,_) <- lookup a n e
        return (Annot a (substT subs t) [], n)

  dischargeInfo q cons subs =
    DischargeInfo (annResult (annotOfExp q)) cons subs

  dischargeF q subs cons =
    case dischargeCS' dischargeC'toplevel cons of
      Left errs
        -> genHoistEither
        $ errorNoSuggestions (ErrorConstraintsNotSatisfied (annAnnot (annotOfExp q)) errs)
      Right (sub, cons')
       -> do let subs'     = compose subs sub
             let q'        = substTX sub q
             let log_ppr   = pretty fun
             let log_info0 = dischargeInfo q cons subs
             let log_info1 = dischargeInfo q' cons' subs'
             checkLog $ CheckLogDischargeOk log_ppr log_info0 log_info1
             return (q', subs', cons')



checkKey
  :: (Hashable n, Eq n, Pretty n)
  => Map.Map (Name n) (Scheme n)
  -> Exp a n
  -> EitherT (CheckError a n) (Fresh.Fresh n) (Exp (Annot a n) n)
checkKey keyEnv expr = do
  q'   <- defaults <$> constraintsQ keyEnv (Query [] expr)
  let t = annResult $ annotOfQuery q'
  case getTemporalityOrPure t of
    TemporalityElement
      -> return ()
    _ -> hoistEither
      $ errorSuggestions
          (ErrorKeyNotElement (annotOfExp expr) t)
          [Suggest "The key must be an element, otherwise, it couldn't be calculated for each value."]

  pure $ final q'
