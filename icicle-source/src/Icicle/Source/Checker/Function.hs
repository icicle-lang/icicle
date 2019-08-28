-- | Typecheck and generalise functions
{-# LANGUAGE DoAndIfThenElse  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE LambdaCase        #-}
module Icicle.Source.Checker.Function (
    checkF
  , checkFs

  , checkF'
  ) where

import                  Icicle.Source.Checker.Base
import                  Icicle.Source.Checker.Error
import                  Icicle.Source.Checker.Constraint

import                  Icicle.Source.Query
import                  Icicle.Source.Type

import                  Icicle.Common.Base
import qualified        Icicle.Common.Fresh     as Fresh
import                  Icicle.Internal.Pretty (Pretty, pretty)

import                  P

import qualified        Data.Map                as Map
import qualified        Data.Set                as Set
import qualified        Data.List               as List
import                  Data.Hashable           (Hashable)

import                  X.Control.Monad.Trans.Either

type Funs a n = [((a, Name n), Exp a n)]
type FunEnvT a n = [ ResolvedFunction a n ]

checkFs :: (Hashable n, Eq n, Pretty n)
        => FunEnvT a n
        -> Funs a n
        -> EitherT (CheckError a n) (Fresh.Fresh n)
                   (FunEnvT a n, [[CheckLog a n]])

checkFs env functions
 = foldlM go (env,[]) functions
 where
  go (env0,logs0) (name,fun)
   = do
    let envMap = Map.fromList $ fmap ((,) <$> functionName <*> functionType) env0
    ((annotfun, funtype),logs') <- checkF envMap fun
    if List.elem (snd name) (fmap functionName env0)
    then hoistEither $ Left $ CheckError (ErrorDuplicateFunctionNames (fst name) (snd name)) []
    else pure (env0 <> [ResolvedFunction (snd name) funtype annotfun], logs0 <> [logs'])

checkF  :: (Hashable n, Eq n, Pretty n)
        => Map.Map (Name n) (Type n)
        -> Exp a n
        -> EitherT (CheckError a n) (Fresh.Fresh n)
                   ((Exp (Annot a n) n, Type n), [CheckLog a n])

checkF env fun
 = evalGen $ checkF' fun env

