{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Icicle.Source.Query.Function (
    Function (..)
  , ResolvedFunction (..)

  , reannotF
  , builtinDefinitions
  ) where

import                  Data.Hashable
import                  Data.String

import                  Icicle.Common.Base
import qualified        Icicle.Common.Fresh         as Fresh

import                  Icicle.Internal.Pretty
import                  Icicle.Source.Query.Builtin
import                  Icicle.Source.Query.Prim
import                  Icicle.Source.Query.Query
import                  Icicle.Source.Query.Exp
import                  Icicle.Source.Type

import                  P

data Function a n
  = Function
  { arguments :: [(a,Name n)]
  , body      :: Query a n }
  deriving (Show, Eq)

reannotF :: (a -> a') -> Function a n -> Function a' n
reannotF f fun
 = fun { arguments = fmap (first f) (arguments fun)
       , body = reannotQ f (body fun) }

data ResolvedFunction a n =
  ResolvedFunction {
     functionName :: Name n
   , functionType :: FunctionType n
   , functionDefinition :: Function (Annot a n) n
   } deriving (Eq, Show)

-- | Build a function environment containing our builtin functions.
builtinDefinitions :: (IsString n, Hashable n, Eq n) => a -> Fresh.Fresh n [ResolvedFunction a n]
builtinDefinitions a_fresh = do
  traverse (buildResolved a_fresh) listOfIntroducedFuns

-- | Build an individual function from the its primitive definition.
--   This is a little bit tricky, as we can't under apply function
--   definitions, so we need to create the arguments to the function
--   as well. It's as if we wrote something like this in the prelude
--   @
--   sin x = sin# x
--   @
--   and then type checked it.
buildResolved :: (IsString n, Hashable n) => a -> BuiltinFun -> Fresh.Fresh n (ResolvedFunction a n)
buildResolved a_fresh builtin = do
  typ
    <- primLookup' (Fun builtin)

  args
    <- for (functionArguments typ) $ \ty -> do
         (Annot a_fresh ty [],) <$> Fresh.fresh

  let
    name
      = nameOf . NameBase . fromString . show . pretty $ builtin
    constraints
      = (a_fresh,) <$> functionConstraints typ
    annot
      = Annot a_fresh (functionReturn typ) constraints
    prim
      = Prim annot (Fun builtin)
    exp'
      = foldl (\ex (a, argName) -> mkApp ex (Var a argName)) prim args
    query'
      = Query [] exp'
    definition
      = Function args query'

  return $
    ResolvedFunction name typ definition

instance Pretty n => Pretty (Function a n) where
  pretty q =
    let
      args =
        case reverse $ arguments q of
          [] ->
            [ prettyPunctuation "=" ]
          (_, n0) : xs0 ->
            fmap (\(_, n) -> pretty n) (reverse xs0) <>
            [ pretty n0 <+> prettyPunctuation "=" ]
    in
      vsep $
        args <> [
            indent 2 . pretty $ body q
          ]
