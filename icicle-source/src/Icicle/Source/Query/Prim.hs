-- | Types of primitives.
-- It is odd that we need to look up inside a Fresh monad.
-- However, because the types are polymorphic in the variable type, we have no way of saying "forall a".
-- So we must generate a fresh name for any forall binders.
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Source.Query.Prim (
    primLookup'
  , primReturnsPossibly
  ) where

import                  Icicle.Source.Query.Constructor
import                  Icicle.Source.Query.Exp
import                  Icicle.Source.Query.Operators
import                  Icicle.Source.Type

import qualified        Icicle.Common.Fresh         as Fresh

import                  P

import                  Data.Hashable (Hashable)


primLookup' :: Hashable n => Prim -> Fresh.Fresh n (FunctionType n)
primLookup' prim
 = case prim of
    -- Negate on Doubles will not introduce NaN or Inf
    Op (ArithUnary Negate)
     -> fNumDefinitely $ \at -> ([at], at)

    -- Pretty much any binary operation on Doubles might return NaN or Inf. This includes addition when the numbers are very large.
    Op (ArithBinary _)
     -> fNumPossibly $ \at -> ([at, at], at)

    -- Division will almost certainly return NaN or Inf
    Op (ArithDouble Div)
     -> f0 [DoubleT, DoubleT] possiblyDouble

    -- Pow can return Inf for large powers
    Op (ArithDouble Pow)
     -> f0 [DoubleT, DoubleT] possiblyDouble


    Op (Relation _)
     -> f1 $ \a at -> functionType [a] [] [at, at] BoolT

    Op (LogicalUnary _)
     -> f0 [BoolT] BoolT
    Op (LogicalBinary _)
     -> f0 [BoolT, BoolT] BoolT
    Op (TimeBinary _)
     -> f0 [IntT, TimeT] TimeT

    Op  TupleComma
     -> do a <- Fresh.fresh
           b <- Fresh.fresh
           let at = TypeVar a
           let bt = TypeVar b
           return $ functionType [a,b] [] [at, bt] (PairT at bt)

    -- Literals will not be NaN or Inf
    Lit (LitInt _)
     -> fNumDefinitely $ \at -> ([], at)
    Lit (LitDouble _)
     -> f0 [] DoubleT
    Lit (LitString _)
     -> f0 [] StringT
    Lit (LitTime _)
     -> f0 [] TimeT

    -- Most Double operations can introduce NaN or Inf
    Fun (BuiltinMath Log)
     -> f0 [DoubleT] possiblyDouble
    Fun (BuiltinMath Exp)
     -> f0 [DoubleT] possiblyDouble
    Fun (BuiltinMath Sqrt)
     -> f0 [DoubleT] possiblyDouble
    Fun (BuiltinMath Acos)
     -> f0 [DoubleT] possiblyDouble
    Fun (BuiltinMath Asin)
     -> f0 [DoubleT] possiblyDouble
    Fun (BuiltinMath Atan)
     -> f0 [DoubleT] possiblyDouble
    Fun (BuiltinMath Atan2)
     -> f0 [DoubleT, DoubleT] possiblyDouble
    Fun (BuiltinMath Cos)
     -> f0 [DoubleT] possiblyDouble
    Fun (BuiltinMath Cosh)
     -> f0 [DoubleT] possiblyDouble
    Fun (BuiltinMath Sin)
     -> f0 [DoubleT] possiblyDouble
    Fun (BuiltinMath Sinh)
     -> f0 [DoubleT] possiblyDouble
    Fun (BuiltinMath Tan)
     -> f0 [DoubleT] possiblyDouble
    Fun (BuiltinMath Tanh)
     -> f0 [DoubleT] possiblyDouble

    -- But conversions are OK
    Fun (BuiltinMath ToDouble)
     -> fNumDefinitely $ \at -> ([at], DoubleT)
    Fun (BuiltinMath Abs)
     -> fNumDefinitely $ \at -> ([at], at)
    Fun (BuiltinMath Floor)
     -> fNumDefinitely $ \at -> ([at], IntT)
    Fun (BuiltinMath Ceiling)
     -> fNumDefinitely $ \at -> ([at], IntT)
    Fun (BuiltinMath Round)
     -> fNumDefinitely $ \at -> ([at], IntT)
    Fun (BuiltinMath Truncate)
     -> fNumDefinitely $ \at -> ([at], IntT)


    Fun (BuiltinText StrLen)
     -> f0 [StringT] (Possibility PossibilityPossibly IntT)
    Fun (BuiltinText ToLower)
     -> f0 [StringT] (Possibility PossibilityPossibly StringT)
    Fun (BuiltinText ToUpper)
     -> f0 [StringT] (Possibility PossibilityPossibly StringT)

    Fun (BuiltinTime DaysBetween)
     -> f0 [TimeT, TimeT] IntT
    Fun (BuiltinTime DaysJulianEpoch)
     -> f0 [TimeT] IntT
    Fun (BuiltinTime SecondsBetween)
     -> f0 [TimeT, TimeT] IntT
    Fun (BuiltinTime SecondsJulianEpoch)
     -> f0 [TimeT] IntT
    Fun (BuiltinTime ProjectDay)
     -> f0 [TimeT] IntT
    Fun (BuiltinTime ProjectMonth)
     -> f0 [TimeT] IntT
    Fun (BuiltinTime ProjectYear)
     -> f0 [TimeT] IntT

    Fun (BuiltinData Seq)
     -> f2 $ \a at b bt -> functionType [a,b] [] [at,bt] bt
    Fun (BuiltinData Box)
     -> f1 $ \a at -> functionType [a] [] [SumT ErrorT at] (Possibility PossibilityPossibly at)

    Fun (BuiltinArray ArraySort)
     -> f1 $ \a at -> functionType [a] [] [ArrayT at] (ArrayT at)
    Fun (BuiltinArray ArrayLength)
     -> f1 $ \a at -> functionType [a] [] [ArrayT at] IntT
    Fun (BuiltinArray ArrayIndex)
     -> f1 $ \a at -> functionType [a] [] [ArrayT at, IntT] (Possibility PossibilityPossibly at)

    Fun (BuiltinMap MapKeys)
     -> f2 $ \a at b bt -> functionType [a,b] [] [GroupT at bt] (ArrayT at)
    Fun (BuiltinMap MapValues)
     -> f2 $ \a at b bt -> functionType [a,b] [] [GroupT at bt] (ArrayT bt)
    Fun (BuiltinMap MapCreate)
     -> f2 $ \k kt v vt -> functionType [k, v] [] [] (GroupT kt vt)
    Fun (BuiltinMap MapInsert)
     -> f2 $ \k kt v vt -> functionType [k, v] [] [kt, vt, GroupT kt vt] (Possibility PossibilityPossibly (GroupT kt vt))
    Fun (BuiltinMap MapDelete)
     -> f2 $ \k kt v vt -> functionType [k, v] [] [kt, GroupT kt vt] (GroupT kt vt)
    Fun (BuiltinMap MapLookup)
     -> f2 $ \k kt v vt -> functionType [k, v] [] [kt, GroupT kt vt] (OptionT vt)

    PrimCon ConSome
     -> f1 $ \a at -> functionType [a] [] [at] (OptionT at)

    PrimCon ConNone
     -> f1 $ \a at -> functionType [a] [] [] (OptionT at)

    PrimCon ConTuple
     -> f2 $ \a at b bt -> functionType [a, b] [] [at, bt] (PairT at bt)

    PrimCon ConTrue
     -> f0 [] BoolT
    PrimCon ConFalse
     -> f0 [] BoolT

    PrimCon ConLeft
     -> f2 $ \a at b bt -> functionType [a, b] [] [at] (SumT at bt)
    PrimCon ConRight
     -> f2 $ \a at b bt -> functionType [a, b] [] [bt] (SumT at bt)

    PrimCon (ConError _)
     -> f0 [] ErrorT

 where
  functionType foralls constraints arguments simpleType
   = FunctionType
   $ TypeForall foralls constraints
   $ foldr TypeArrow simpleType arguments

  f0 argsT resT
   = return $ functionType [] [] argsT resT

  -- A num operation that can introduce NaN or Inf and must be checked
  fNumPossibly f
   = fNum $ \pt at ->
     let (args,ret) = f at
     in  (args, Possibility pt ret)

  -- Safe number operations
  fNumDefinitely f
   = f1 $ \a at ->
     let (args,ret) = f at
     in  functionType [a] [CIsNum at] args ret

  fNum f
   = f2 (\a at p pt -> uncurry (functionType [a,p] [CPossibilityOfNum pt at]) (f pt at))

  possiblyDouble = Possibility PossibilityPossibly DoubleT

  f1 f
   = do n <- Fresh.fresh
        return $ f n (TypeVar n)

  f2 f
   = do n1 <- Fresh.fresh
        n2 <- Fresh.fresh
        return $ f n1 (TypeVar n1) n2 (TypeVar n2)


-- There must be a better way.
-- When an expression returns Possibly, it is hard to know whether that's because the function returns Possibly,
-- or if it's a pure function applied to a Possibly argument which needs to be reboxed.
-- This should probably be figured out and inserted during type inference.
-- As it is, we're looking at the expression and the result of type inference to decide - trying to work backwards to
-- figure out what inference did.
primReturnsPossibly :: Prim -> Type n -> Bool
primReturnsPossibly (Fun (BuiltinData  Box))        _ = True
primReturnsPossibly (Fun (BuiltinMap   MapInsert))  _ = True
primReturnsPossibly (Fun (BuiltinArray ArrayIndex)) _ = True
primReturnsPossibly (Fun (BuiltinText  _))          _ = True
primReturnsPossibly p ty
 | (_, pos, dat)       <- decomposeT ty
 , DoubleT             <- dat
 , Just PossibilityPossibly <- pos
 = case p of
    Op (ArithBinary _)     -> True
    Op (ArithDouble Div)   -> True
    Op (ArithDouble Pow)   -> True
    Fun (BuiltinMath Log)  -> True
    Fun (BuiltinMath Exp)  -> True
    Fun (BuiltinMath Sqrt) -> True
    Fun (BuiltinMath Acos) -> True
    Fun (BuiltinMath Asin) -> True
    Fun (BuiltinMath Atan) -> True
    Fun (BuiltinMath Atan2)-> True
    Fun (BuiltinMath Cos)  -> True
    Fun (BuiltinMath Cosh) -> True
    Fun (BuiltinMath Sin)  -> True
    Fun (BuiltinMath Sinh) -> True
    Fun (BuiltinMath Tan)  -> True
    Fun (BuiltinMath Tanh) -> True
    _                      -> False
primReturnsPossibly _ _                          = False

