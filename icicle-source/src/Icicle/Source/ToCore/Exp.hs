-- | Convert Element Expressions to Core
--
-- Worker functions, eg the "value * 2" in "sum (value * 2)".
-- Aggregates are dealt with elsewhere.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Source.ToCore.Exp (
    convertExp
  , convertExpQ
  , convertCase
  , convertCaseFreshenPat
  , convertAccess

  , isAnnotPossibly
  , unwrapSum
  , rewrapSum
  ) where

import                  Icicle.Source.Query
import                  Icicle.Source.ToCore.Base
import                  Icicle.Source.ToCore.Prim
import                  Icicle.Source.Type

import qualified        Icicle.Core as C
import qualified        Icicle.Core.Exp.Combinators as CE
import qualified        Icicle.Common.Exp           as CE
import qualified        Icicle.Common.Exp.Prim.Minimal as Min
import qualified        Icicle.Common.Type          as T
import                  Icicle.Common.Base
import                  Icicle.Common.Fresh

import                  P

import                  Control.Monad.Trans.Class

import                  Data.List (zip)
import qualified        Data.Map as Map
import                  Data.Hashable (Hashable)

-- | Convert an element-level expression.
-- These are worker functions for folds, filters and so on.
convertExp
        :: (Hashable n, Eq n)
        => Exp (Annot a n) n
        -> ConvertM a n (C.Exp () n)
convertExp x
 = case x of
    Var ann n
     -> do  bound <- convertFreshenLookupMaybe n
            fs    <- featureContextVariables <$> convertFeatures
            case bound of
             Just fv
              -> return $ CE.xVar fv
             _
              | Just fv <- Map.lookup n fs
              -> (featureVariableExp fv . CE.xVar) <$> convertInputName
              | otherwise
              -> convertError $ ConvertErrorExpNoSuchVariable (annAnnot ann) n

    Nested _ q
     -> convertExpQ q


    App ann f q
     -- Primitive application: convert arguments, then convert primitive
     | Just (p, Annot { annResult = resT }, args) <- takePrimApps x
     -> do  args'   <- mapM convertExp args
            let tys  = fmap (annResult . annotOfExp) args
            convertPrim p (annAnnot ann) resT (args' `zip` tys)

     | otherwise
     -> do  p'      <- convertExp f
            q'      <- convertExp q
            return $
              p' `CE.xApp` q'

    Prim ann p
     -> convertPrim p (annAnnot ann) (annResult ann) []

    Lam ann n p
     -> do  vArgT   <- convertFunctionArgType (annAnnot ann) (annResult ann)
            p'      <- convertWithInput n vArgT (convertExp p)
            return $
              CE.xLam n vArgT p'

    If ann scrut true false
     -> do  scrut' <- convertExp scrut
            true'  <- convertExp true
            false' <- convertExp false
            resT   <- convertValType (annAnnot ann) $ annResult ann
            sn     <- lift fresh
            return
              (CE.xPrim (C.PrimFold C.PrimFoldBool resT)
                CE.@~ CE.xLam sn T.UnitT true'
                CE.@~ CE.xLam sn T.UnitT false'
                CE.@~ scrut')

    -- Only deal with flattened, single layer cases.
    -- We need a pass beforehand to simplify them.
    Case ann scrut pats
     -> do  scrut' <- convertExp scrut
            pats'  <- mapM goPat pats
            scrutT <- convertValType (annAnnot ann) $ annResult $ annotOfExp scrut
            resT   <- convertValType (annAnnot ann) $ annResult ann
            convertCase x scrut' pats' scrutT resT

    Access ann xpression field
     -> do  xpression' <- convertExp xpression
            xpressionT <- convertValType (annAnnot ann) $ annResult $ annotOfExp xpression
            convertAccess ann xpression' xpressionT field

    Record ann fields
     -> do  fields'    <- Map.fromList <$> traverse (traverse convertExp) fields
            fieldsT    <- convertValType (annAnnot ann) $ annResult ann
            structT    <- case fieldsT of
                            T.StructT st ->
                              return st
                            _ ->
                              convertError
                                $ ConvertErrorInputTypeNotMap (annAnnot ann) fieldsT
            return $ CE.makeApps () (CE.xPrim $ C.PrimMinimal $ Min.PrimStruct $ Min.PrimStructConstruct structT) (Map.elems fields')


 where
  goPat (p,alt)
   = convertContext
   $ do p'   <- convertCaseFreshenPat p
        alt' <- convertExp alt
        return (p', alt')


convertExpQ
        :: (Hashable n, Eq n)
        => Query (Annot a n) n
        -> ConvertM a n (C.Exp () n)
convertExpQ q
 -- Remove any new bindings from context afterwards
 = convertContext
 $ case contexts q of
    []
     -> convertExp $ final q

    (Let _ (PatVariable b) d:cs)
     -> do  d' <- convertExp d
            -- NB: because it's non-recursive let, the freshen must be done after the definition
            b' <- convertFreshenAdd b
            x' <- convertExpQ $ Query cs $ final q
            return $ CE.xLet b' d' x'

    (LetScan _ _ _:cs)
     -> do  -- NB: We can only reach here if the query is pure; in which case the element from
            -- the scan can't be used. Just ignore it.
            convertExpQ $ Query cs $ final q
    _
     -> convertError
      $ ConvertErrorExpNestedQueryNotAllowedHere (annAnnot $ annotOfQuery q) q

-- | Enfreshinate the variables in a case pattern and add them to the convert environment.
--
convertCaseFreshenPat :: (Hashable n, Eq n) => Pattern n -> ConvertM a n (Pattern n)
convertCaseFreshenPat p
 = case p of
    PatCon c ps
      -> PatCon c <$> mapM convertCaseFreshenPat ps
    PatDefault
      -> return PatDefault
    PatLit l n
      -> return $ PatLit l n
    PatVariable n
      -> PatVariable <$> convertFreshenAdd n


convertCase
        :: Hashable n
        => Exp (Annot a n) n
        -> C.Exp () n
        -> [(Pattern n, C.Exp () n)]
        -> T.ValType
        -> T.ValType
        -> ConvertM a n (C.Exp () n)
convertCase x scrut pats scrutT resT
 = do   sn <- lift fresh
        m <- convertConstructorMap
        case scrutT of
         T.OptionT ta
          | Just ([n],som) <- Map.lookup ConSome    m
          , Just ([],non)  <- Map.lookup ConNone    m

          -> return ((CE.xPrim $ C.PrimFold (C.PrimFoldOption ta) resT)
                     CE.@~ (CE.xLam n ta som) CE.@~ (CE.xLam sn T.UnitT non)
                     CE.@~ scrut)

         T.BoolT
          | Just ([],tru) <- Map.lookup ConTrue     m
          , Just ([],fal) <- Map.lookup ConFalse    m
          -> return ((CE.xPrim $ C.PrimFold C.PrimFoldBool resT)
                     CE.@~ CE.xLam sn T.UnitT tru
                     CE.@~ CE.xLam sn T.UnitT fal
                     CE.@~ scrut)

         T.PairT ta tb
          | Just ([na,nb],tup) <- Map.lookup ConTuple   m
          , xfst <- CE.xPrim (C.PrimMinimal $ Min.PrimPair $ Min.PrimPairFst ta tb) CE.@~ CE.xVar sn
          , xsnd <- CE.xPrim (C.PrimMinimal $ Min.PrimPair $ Min.PrimPairSnd ta tb) CE.@~ CE.xVar sn
          -> return ( CE.xLet sn scrut
                    $ CE.xLet na xfst
                    $ CE.xLet nb xsnd
                    $ tup)

         T.SumT ta tb
          | Just ([nl],xl)  <- Map.lookup ConLeft    m
          , Just ([nr],xr)  <- Map.lookup ConRight   m

          -> return ((CE.xPrim $ C.PrimFold (C.PrimFoldSum ta tb) resT)
                     CE.@~ (CE.xLam nl ta xl)
                     CE.@~ (CE.xLam nr tb xr)
                     CE.@~ scrut)

         T.UnitT
          | Just ([],unit) <- Map.lookup ConUnit     m
          -> return unit

         other
          -> convertError $ ConvertErrorBadCaseNoDefault (annAnnot $ annotOfExp x) other x
 where
  convertConstructorMap
   = Map.fromList <$> mapM mkPatMap pats

  mkPatMap (PatCon c ps, alt)
   = do ps'  <- mapM mkVars ps
        return (c, (ps', alt))
  mkPatMap _
   = convertError $ ConvertErrorBadCaseNoDefault (annAnnot $ annotOfExp x) T.UnitT x

  mkVars PatDefault
   = lift fresh
  mkVars (PatVariable n)
   = return n
  mkVars (PatCon _ _)
   = convertError $ ConvertErrorBadCaseNestedConstructors (annAnnot $ annotOfExp x) x
  mkVars (PatLit _ _)
   = convertError $ ConvertErrorBadCaseNestedConstructors (annAnnot $ annotOfExp x) x


convertAccess
  :: Annot a n
  -> C.Exp () n
  -> T.ValType
  -> StructField
  -> ConvertM a n (C.Exp () n)
convertAccess ann xpression' xpressionT field =
  case xpressionT of
    T.StructT st@(T.StructType struct'map)
      | Just fieldType <- Map.lookup field struct'map
      -> return
          (CE.xPrim (C.PrimMinimal $ Min.PrimStruct $ Min.PrimStructGet field fieldType st)
            CE.@~ xpression')

    T.TimeT
      | field == T.StructField "year"
      -> return
          (CE.xPrim (C.PrimMinimal $ Min.PrimTime Min.PrimTimeProjectYear)
            CE.@~ xpression')

    T.TimeT
      | field == T.StructField "month"
      -> return
          (CE.xPrim (C.PrimMinimal $ Min.PrimTime Min.PrimTimeProjectMonth)
            CE.@~ xpression')

    T.TimeT
      | field == T.StructField "day"
      -> return
          (CE.xPrim (C.PrimMinimal $ Min.PrimTime Min.PrimTimeProjectDay)
            CE.@~ xpression')

    _ -> convertError
        $ ConvertErrorCannotConvertAccessor (annAnnot ann) xpressionT field



isAnnotPossibly :: Annot a n -> Bool
isAnnotPossibly ann
 = case getPossibilityOrDefinitely (annResult ann) of
    PossibilityPossibly -> True
    _                   -> False


unwrapSum
    :: Bool
    -> T.ValType
    -> Name n
    -> C.Exp () n
    -> Name n
    -> T.ValType
    -> C.Exp () n
    -> C.Exp () n
unwrapSum isPossibly rett nErr x nk t bodyx
 | T.SumT T.ErrorT ty <- t
 , T.SumT T.ErrorT ret' <- rett
 -- We can only do this for (Sum Error)s introduced by Reify:
 -- not ones that the programmer explicitly wrote
 , isPossibly
 = CE.makeApps () (CE.xPrim $ C.PrimFold (C.PrimFoldSum T.ErrorT ty) rett)
 [ CE.xLam nErr T.ErrorT ( CE.makeApps () (CE.xPrim $ C.PrimMinimal $ Min.PrimConst $ Min.PrimConstLeft T.ErrorT ret')
                         [ CE.xVar nErr ])
 , CE.xLam nk ty bodyx
 , x ]
 | otherwise
 = CE.xLet nk x bodyx


rewrapSum
    :: Bool
    -> T.ValType
    -> C.Exp () n
    -> C.Exp () n
rewrapSum isPossibly rett bodyx
 | T.SumT T.ErrorT ret' <- rett
 , isPossibly
 = CE.makeApps () (CE.xPrim $ C.PrimMinimal $ Min.PrimConst $ Min.PrimConstRight T.ErrorT ret')
 [ bodyx ]
 | otherwise
 = bodyx
