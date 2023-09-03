{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Source.Parser.Constructor (
    checkPat
  , constructors
  ) where

import qualified        Icicle.Source.Query    as Q

import                  Icicle.Common.Base

import                  Data.String (IsString)

import                  P hiding (exp)

constructors :: [(Text, Q.Constructor)]
constructors
 = [("Some", Q.ConSome)
   ,("None", Q.ConNone)
   ,("True", Q.ConTrue)
   ,("False",Q.ConFalse)
   ,("Left", Q.ConLeft)
   ,("Right",Q.ConRight)
   ,("ExceptTombstone",         Q.ConError ExceptTombstone)
   ,("ExceptFold1NoValue",      Q.ConError ExceptFold1NoValue)
   ,("ExceptCannotCompute",     Q.ConError ExceptCannotCompute)
   ,("ExceptNotANumber",        Q.ConError ExceptNotANumber)
   ,("ExceptIndexOutOfBounds",  Q.ConError ExceptIndexOutOfBounds)
   ]


-- | Convert an expression to a pattern.
--
--   This is used in the parsing stage when a pattern
--   is required, we parse as an expression, then coerce
--   to a pattern.
--
--   Obviously, not all expressions can be converted
--   in this way, but all valid patterns can be parsed
--   as an expression.
--
--   Going from an expression to a pattern instead of
--   using a separate parser for patterns has the benefit
--   that quite tricky things like tuple comma fixity is
--   handled the same in the patterns as the expressions
--   they match, and we don't have to duplicate parser
--   logic.
checkPat :: (IsString v, Eq v, Show v, MonadFail m) => Q.Exp pos v -> m (Q.Pattern v)
checkPat exp =
  case exp of
    -- Variables are simple, just underscore default
    -- to a default pattern if required, and leave all
    -- the rest alone
    Q.Var _ v
      | nameBase v == NameBase "_"
      -> return Q.PatDefault
      | otherwise
      -> return $ Q.PatVariable v

    -- Applications
    Q.App {}
      -- Simple contructors are easy
      | Just (p, _, xs) <- Q.takePrimApps exp
      , Q.PrimCon con  <- p
      -> Q.PatCon con <$> traverse checkPat xs

      -- Negative literals are trickier.
      -- We need to ensure that the rest of the
      -- pattern is a number literal.
      -- Matching on False as well ensures that
      -- '-(-4)' won't be regarded as a correct
      -- pattern.
      | Just (p, _, xs) <- Q.takePrimApps exp
      , Q.Op (Q.ArithUnary Q.Negate) <- p
      -> traverse checkPat xs >>= \case
           [Q.PatLit l@(Q.LitInt _) False] -> return $ Q.PatLit l True
           [Q.PatLit l@(Q.LitDouble _) False] -> return $ Q.PatLit l True
           _ -> fail "unable to parse pattern, non numeric literals can't be negative"

      | otherwise
      -> fail $ "unable to parse application as a pattern" <> show (Q.reannot (const ()) exp)

    -- Primitives can be empty constructors
    -- and literals.
    Q.Prim _ p
      | Q.PrimCon con  <- p
      -> return $ Q.PatCon con []
      | Q.Lit lit <- p
      -> return $ Q.PatLit lit False
      | otherwise
      -> fail "unable to parse pattern"

    Q.Lam {}
      -> fail "unable to parse lambda function as a pattern"

    Q.Nested {}
      -> fail "unable to parse nested queries as a pattern"

    Q.Case {}
      -> fail "unable to parse case expressions as a pattern"

    Q.If {}
      -> fail "unable to parse if expression as a pattern"

    Q.Access {}
      -> fail "unable to parse record access as a pattern"

    Q.Record {}
      -> fail "unable to parse record constructor as a pattern"
