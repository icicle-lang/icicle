{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Source.Parser.Operators (
    defix
  , renderDefixError
  , DefixError(..)
  ) where

import qualified        Icicle.Source.Lexer.Token       as T
import qualified        Icicle.Source.Query             as Q
import                  Icicle.Source.Query.Operators

import                  P

import                  Data.String (String)
import                  Data.Text (unpack)

data DefixError pos n
 = ErrorNoSuchPrefixOperator          Text
 | ErrorNoSuchInfixOperator           Text
 | ErrorExpectedExpressionGotEnd
 | ErrorExpectedOperatorGotExpression (Q.Exp pos n)
 | ErrorBUGPrefixInCrunch
 | ErrorBUGLeftovers [Q.Exp pos n] [(Q.Op, pos)]
 deriving (Show, Eq, Ord)

renderDefixError :: (Show pos, Show n) => DefixError pos n -> String
renderDefixError e
 | ErrorNoSuchPrefixOperator op <- e
 , OpsOfSymbol (Just _) _ <- symbol op
 = "the infix operator \"" <> unpack op <> "\" was used in a prefix position."
 | ErrorNoSuchPrefixOperator op <- e
 = "no such prefix operator \"" <> unpack op <> "\" exists."
 | ErrorNoSuchInfixOperator op <- e
 , OpsOfSymbol _ (Just _) <- symbol op
 = "the prefix operator \"" <> unpack op <> "\" was used in an infix position."
 | ErrorNoSuchInfixOperator op <- e
 = "no such infix operator \"" <> unpack op <> "\" exists."
 | ErrorExpectedExpressionGotEnd <- e
 = "no postfix operators are supported. Expected the expression to continue."
 | otherwise
 = show e

data Ops
 = Ops Text OpsOfSymbol
 deriving (Show, Eq, Ord)


-- | Convert from infix operators to ast - "de-infixing"
defix :: [Either (Q.Exp pos n) (T.Operator, pos)] -> Either (DefixError pos n) (Q.Exp pos n)
defix inps
 = shuntX [] []
 $ fmap (second get) inps
 where
  -- Look up operators by symbol.
  -- There can be multiple operators for a given symbol,
  -- but one must be prefix and the other infix.
  -- (ie "-" means negation and subtraction)
  get (T.Operator sym, pos)
   = (Ops sym $ symbol sym, pos)


-- | Shunting-yard algorithm
-- With a slight modification for prefix operators.
--
-- We keep two stacks of things we've seen: one for expressions and one for operators.
--
-- When we see an operator, check it against what's on the top of the operator stack.
-- If the precedence is lower than the top of the stack, then the stack operator should
-- apply first, so apply the stack operator to the two top expressions,
-- and then push the new operator onto the operator stack.
--
-- However, this doesn't deal with prefix operators.
-- Prefixes can only occur at the start of the expression, or directly
-- after another operator.
-- We keep track of whether we're expecting to see an expression or an infix operator
-- right now, and if we see an operator when we're expecting an expression, it must be a prefix.
--
-- This is why it is split into two functions - shuntX for expressions of prefixes,
-- shuntI for infix operators.

shuntX  :: [Q.Exp pos n]
        -- ^ The expression stack
        -> [(Q.Op, pos)]
        -- ^ The operators stack - binary operators only
        -> [Either (Q.Exp pos n) (Ops, pos)]
        -- ^ The inputs
        -> Either (DefixError pos n) (Q.Exp pos n)

-- Try to grab an expression off the front and proceed with infixes
shuntX xs os inps
 = do   (x,inps') <- shuntPrefix inps
        shuntI (x:xs) os inps'


-- | Try to grab an expression off the front, return it and the remaining input
shuntPrefix
        :: [Either (Q.Exp pos n) (Ops, pos)]
        -> Either (DefixError pos n) (Q.Exp pos n, [Either (Q.Exp pos n) (Ops, pos)])

shuntPrefix []
 = Left $ ErrorExpectedExpressionGotEnd

shuntPrefix (Left x : inps)
 = do   let (xs,inps') = exprs inps
        let x' = foldl Q.mkApp x xs
        return (x', inps')
 where
  exprs (Left a : rs)
   = let (xs, rs') = exprs rs
     in  (a : xs, rs')

  exprs rs
   = ([],rs)

shuntPrefix (Right (Ops sym ops, pos) : inps)
 = case opPrefix ops of
    Just o
     -> do  (x, inps') <- shuntPrefix inps
            return (Q.Prim pos (Q.Op o) `Q.mkApp` x, inps')
    Nothing
     -> Left $ ErrorNoSuchPrefixOperator sym


-- | Shunt an infix operator
shuntI  :: [Q.Exp pos n]
        -- ^ The expression stack
        -> [(Q.Op, pos)]
        -- ^ The operators stack - binary operators only
        -> [Either (Q.Exp pos n) (Ops, pos)]
        -- ^ The inputs
        -> Either (DefixError pos n) (Q.Exp pos n)
shuntI xs os []
 = finish xs os

shuntI _xs _os (Left x : _)
 -- TODO: this should generate an application, but should actually be handled by shuntPrefix.
 -- This whole thing is structured incorrectly
 = Left $ ErrorExpectedOperatorGotExpression x

shuntI xs os (Right (Ops sym ops, pos) : inps)
 -- Just get the infix ones
 = case opInfix ops of
    Just o
     -> do  (xs',os') <- crunchOperator xs os $ fixity o
            shuntX xs' ((o,pos):os') inps
    Nothing
     -> Left $ ErrorNoSuchInfixOperator sym


-- | Prepare the stacks for putting a new operator on top.
-- Depending on the new operator's precedence, we might need to apply
-- top expressions to the top operator
crunchOperator
        :: [Q.Exp pos n]
        -- ^ The expression stack
        -> [(Q.Op, pos)]
        -- ^ The operators stack
        -> Fixity
        -- ^ Operator we're about to push
        -> Either (DefixError pos n) ([Q.Exp pos n], [(Q.Op, pos)])

-- If we have two arguments to apply and an operator
crunchOperator (x:y:xs) ((o,pos):os) f
 = case (f, fixity o) of
    (FInfix (Infix a1 p1), FInfix (Infix _ p2))
     -- If the precedence is less, apply the arguments.
     -- Note that this is reverse order, since the list is a stack
     | less a1 p1 p2
     -> let xs' = Q.Prim pos (Q.Op o) `Q.mkApp` y `Q.mkApp` x : xs
        -- Check if we need to keep crunching
        in  crunchOperator xs' os f

     -- Leave it alone
     | otherwise
     -> return (x:y:xs, (o,pos):os)
    _
     -> Left $ ErrorBUGPrefixInCrunch

 where
  -- Look at the associativity of the new operator.
  -- Suppose the existing operator is the same operator:
  -- If it's left associative, the existing operator should gobble up
  -- its inputs and then the operator we push will use the result.
  --
  -- Therefore, equal precedence must crunch.
  less AssocLeft  p1 p2
   = p1 <= p2
  -- But if it's right associative and the same operator is there,
  -- equal precedence should not crunch - we want the existing arguments for
  -- the later operator.
  less AssocRight p1 p2
   = p1 <  p2

-- We don't have arguments or an operator, so leave it alone.
crunchOperator xs os _
 = return (xs,os)


finish  :: [Q.Exp pos n]
        -- ^ The expression stack
        -> [(Q.Op, pos)]
        -- ^ The operators stack
        -> Either (DefixError pos n) (Q.Exp pos n)
finish [x] []
 = return x

finish (x:y:xs) ((o,pos):os)
 = finish (Q.Prim pos (Q.Op o) `Q.mkApp` y `Q.mkApp` x : xs) os

finish xs os
 = Left $ ErrorBUGLeftovers xs os

