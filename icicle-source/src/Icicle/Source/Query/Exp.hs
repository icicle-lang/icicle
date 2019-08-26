-- | Query expressions, like aggregates and operators and stuff
-- Expressions can have nested queries, but putting queries and expressions
-- in the same file just gets too confusing.
-- To break the cycle, we make Exp' take a recursive parameter for the query,
-- and "tie the knot" in Query.
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TupleSections #-}
module Icicle.Source.Query.Exp (
    Exp'      (..)
  , Prim      (..)
  , Lit       (..)
  , Op        (..)
  , Fun
  , BuiltinFun   (..)
  , BuiltinMath  (..)
  , BuiltinText  (..)
  , BuiltinTime  (..)
  , BuiltinData  (..)
  , BuiltinArray (..)
  , BuiltinMap   (..)

  , TraverseAnnot (..)
  , reannot

  , takeApps
  , takePrimApps
  , takeLams
  , makeLams
  , annotOfExp
  , mkApp
  , precedenceOfX
  , listOfIntroducedFuns
  , listOfWiredFuns
  ) where

import           Control.Lens.Setter (over)

import           GHC.Generics (Generic)

import           Icicle.Source.Query.Builtin
import           Icicle.Source.Query.Constructor
import           Icicle.Source.Query.Operators
import           Icicle.Internal.Pretty
import           Icicle.Common.Base

import           P

class TraverseAnnot q where
  traverseAnnot :: Applicative f => (a -> f a') -> q a n -> f (q a' n)

reannot :: TraverseAnnot q => (a -> a') -> q a n -> q a' n
reannot = over traverseAnnot

data Exp' q a n
 = Var a (Name n)
 | Lam a (Name n) (Exp' q a n)
 | Nested a (q a n)
 | App  a (Exp' q a n) (Exp' q a n)
 | Prim a Prim
 | Case a (Exp' q a n) [(Pattern n, Exp' q a n)]
 deriving (Show, Eq, Ord, Generic)

instance (NFData (q a n), NFData a, NFData n) => NFData (Exp' q a n)

instance TraverseAnnot q => TraverseAnnot (Exp' q)  where
  traverseAnnot f xx =
    case xx of
      Var    a n   -> Var    <$> f a <*> pure n
      Lam    a n q -> Lam    <$> f a <*> pure n            <*> traverseAnnot f q
      Nested a q   -> Nested <$> f a <*> traverseAnnot f q
      App    a x y -> App    <$> f a <*> traverseAnnot f x <*> traverseAnnot f y
      Prim   a p   -> Prim   <$> f a <*> pure p
      Case a scrut pats
       -> Case <$> f a <*> traverseAnnot f scrut
               <*> traverse (\(p,x) -> (p,) <$> traverseAnnot f x) pats

takeLams :: Exp' q a n -> ([(a, Name n)], Exp' q a n)
takeLams (Lam a n x) =
  let
    (bs, ret) = takeLams x
    binds = (a, n) : bs
  in
    (binds, ret)
takeLams x = ([], x)

makeLams :: [(a, Name n)] -> Exp' q a n -> Exp' q a n
makeLams ls x = foldr (uncurry Lam) x ls

takeApps :: Exp' q a n -> (Exp' q a n, [Exp' q a n])
takeApps xx
 = case xx of
    App _ f x
     -> let (f', xs) = takeApps f
        in  (f', xs <> [x])
    _
     -> (xx, [])

takePrimApps :: Exp' q a n -> Maybe (Prim, a, [Exp' q a n])
takePrimApps x
 | (Prim a p, xs) <- takeApps x
 = Just (p, a, xs)
 | otherwise
 = Nothing

annotOfExp :: Exp' q a n -> a
annotOfExp x
 = case x of
   Var    a _   -> a
   Lam    a _ _ -> a
   Nested a _   -> a
   App    a _ _ -> a
   Prim   a _   -> a
   Case   a _ _ -> a

mkApp :: Exp' q a n -> Exp' q a n -> Exp' q a n
mkApp x y
 = App (annotOfExp x) x y


data Prim
 = Op Op
 | Lit Lit
 | Fun Fun
 | PrimCon Constructor
 deriving (Show, Eq, Ord, Generic)

instance NFData Prim

-- | Built-in Source functions
type Fun = BuiltinFun

instance (Pretty n, Pretty (q a n)) => Pretty (Exp' q a n) where
  prettyPrec outer_prec xx =
    wrap $
      case xx of
        App{}
         -- Operators
         | Just (Op o, _, [x]) <- takePrimApps xx
         , FPrefix <- fixity o
         -> pretty o <+> prettyPrec inner_prec x
         | Just (Op o, _, [x,y]) <- takePrimApps xx
         , FInfix _ <- fixity o
         ->  prettyPrec inner_prec_1 x
         <+> pretty  o
         <+> prettyPrec inner_prec_2 y

        App _ x y ->
          prettyPrec inner_prec_1 x <+> prettyPrec inner_prec_2 y

        Var _ n ->
          annotate AnnVariable (pretty n)

        Lam _ n x ->
          prettyPunctuation "\\" <> pretty n <+> prettyPunctuation "->" <+> prettyPrec inner_prec_2 x

        Prim _ p ->
          annotate AnnPrimitive (pretty p)

        Nested _ q ->
          pretty q

        Case _ scrut pats ->
          vsep [
              prettyKeyword "case" <+> pretty scrut
            , vcat . with pats $ \(p, x) ->
                vsep [
                    prettyPunctuation "|" <+> pretty p <+> prettyPunctuation "->"
                  , indent 4 $ pretty x
                  ]
            , prettyKeyword "end"
            ]
   where
    (inner_prec, assoc) = precedenceOfX xx

    -- Precedence of first operator argument
    inner_prec_1
     | AssocLeft <- assoc
     = inner_prec
     | otherwise
     = inner_prec + 1

    -- Precedence of second operator argument
    inner_prec_2
     | AssocRight <- assoc
     = inner_prec
     | otherwise
     = inner_prec + 1

    -- Suppose we have
    --
    --   7   6   7   (precedences)
    -- a * b + c * d
    --
    --        +        6
    --      /   \
    --     *     *     7
    --    / \   / \
    --   a   b c   d
    --
    -- So when pretty printing, the precedence is allowed to increase
    -- without requiring parentheses, but decreasing needs them.
    --
    wrap =
      parensWhen (inner_prec < outer_prec)

-- | Find the pretty-printing precedence of an expression.
precedenceOfX :: Exp' q a n -> (Int, Assoc)
precedenceOfX xx
 -- Note: this is assuming that operators will only be applied to one or two arguments,
 -- and that the expression has the right number of arguments.
 | Just (Op o, _, as) <- takePrimApps xx
 = case fixity o of
    FInfix (Infix a p)
     | length as == 2
     -> (p, a)
    FPrefix
     | length as == 1
     -> precedencePrefix

    _
     -> precedenceApplication
 | otherwise
 = case xx of
    Var{}
     -> precedenceNeverParens
    Lam{}
     -> precedenceApplication
    Nested{}
     -> precedenceAlwaysParens
    App{}
     -> precedenceApplication
    Prim _ (Op _)
     -> precedenceAlwaysParens
    Prim{}
     -> precedenceNeverParens
    Case{}
     -> precedenceApplication

instance Pretty Prim where
  pretty = \case
    Op o ->
      pretty o
    Lit l ->
      pretty l
    Fun f ->
      pretty f
    PrimCon c ->
      pretty c
