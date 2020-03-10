-- | Typecheck and generalise functions
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TupleSections #-}
module Icicle.Source.Checker.Function (
    checkF
  , checkModule

  , checkF'
  ) where

import                  Icicle.Source.Checker.Base
import                  Icicle.Source.Checker.Error
import                  Icicle.Source.Checker.Constraint
import                  Icicle.Source.Checker.Subsumption

import                  Icicle.Source.Query
import                  Icicle.Source.Type

import                  Icicle.Common.Base
import qualified        Icicle.Common.Fresh     as Fresh
import                  Icicle.Internal.Pretty (Pretty, pretty)

import                  P

import                  Control.Monad.Trans.Either
import                  Control.Monad.Trans.Class (lift)

import qualified        Data.Map                as Map
import qualified        Data.Set                as Set
import qualified        Data.List               as List
import                  Data.Hashable           (Hashable)


type FunEnvT a n = [ ResolvedFunction a n ]


checkModule
  :: (Hashable n, Eq n, Pretty n)
  => FunEnvT a n
  -> Module a n
  -> EitherT (CheckError a n, [CheckLog a n]) (Fresh.Fresh n)
             (ResolvedModule a n, [[CheckLog a n]])


checkModule env module'
 = do (entries, logs) <- foldlM go (env,[]) (moduleEntries module')
      return $
        (ResolvedModule (moduleName module') (moduleImports module') entries, logs)

 where
  go (env0,logs0) decl@(DeclFun ann name _ _) = do
    let
      env1 =
        env <> env0
      envMap =
        Map.fromList $ fmap ((,) <$> functionName <*> functionType) env1

    (checkResult,logs') <-
      lift $ checkF envMap decl
    (annotfun, funtype) <-
      hoistEither $ first (,logs') checkResult

    if List.elem name (fmap functionName env1) then
      hoistEither $ Left $ (CheckError (ErrorDuplicateFunctionNames ann name) [], [])
    else
      pure (env0 <> [ResolvedFunction name funtype annotfun], logs0 <> [logs'])

checkF
  :: (Hashable n, Eq n, Pretty n)
  => Map.Map (Name n) (Scheme n)
  -> Decl a n
  -> (Fresh.Fresh n) (Either (CheckError a n) (Exp (Annot a n) n, Scheme n), [CheckLog a n])

checkF env (DeclFun a _ t fun)
 = evalGen $ checkF' fun env >>= constrain a t


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
      env' <- foldM bindArg env $ arguments
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

      -- Generalise any modes - temporality or possibility - that are not mentioned in constraints
      let remode t
           | Just (TypeVar n) <- t
           , not $ Set.member n keepModes
           = Nothing
           | otherwise
           = t
     -- Take apart temporality and possibility, remode, then put it back together
      let fixmodes t
           = let (tmp,pos,dat) = decomposeT t
             in  recomposeT (remode tmp, remode pos, dat)

      -- Fix the modes of all the argument and result types
      let argTs = fmap (fixmodes . annResult . fst) args

      let resT  = fixmodes $ annResult $ annotOfExp q

      -- Find free variables in types and constraints - these have to be bound as foralls.
      let binds = Set.toList
                $ Set.unions
                $ keepModes : freeT resT : fmap freeT argTs

      let arrT  = foldr TypeArrow resT argTs
      let lams  = foldr (uncurry Lam) q args

      -- Put it all together
      let funT  = Forall binds constrs arrT

      return (lams, funT)
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
