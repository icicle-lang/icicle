{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
module Icicle.Source.ToCore.Base (
    CoreBinds    (..)
  , ConvertError (..)
  , annotOfError
  , ConvertM
  , ConvertState (..)
  , convertInput
  , convertInputName
  , convertInputType
  , convertDateName
  , convertFactTimeName
  , convertMaxMapSize
  , convertWithInput
  , convertWithInputName
  , convertError
  , convertFeatures
  , convertFreshenAdd
  , convertFreshenAddAs
  , convertFreshenLookup
  , convertFreshenLookupMaybe
  , convertValType
  , convertFunctionArgType
  , convertContext
  , convertAddElementRepack
  , convertElementRepacks

  , pre, filt, sfold, post
  , programOfBinds
  , pullPosts
  , windowEdge
  ) where

import qualified        Icicle.Core             as C
import qualified        Icicle.Core.Exp.Combinators as CE

import                  Icicle.Common.Fresh
import                  Icicle.Common.Base
import                  Icicle.Common.Type  hiding (Type)
import qualified        Icicle.Common.Exp       as X
import qualified        Icicle.Common.Exp.Prim.Minimal as Min

import                  Icicle.Data.Name

import                  Icicle.Source.Query
import                  Icicle.Source.Type

import                  Icicle.Internal.Pretty

import                  P hiding (with)

import                  Control.Monad.Trans.Class
import                  Control.Monad.Trans.State.Lazy


import qualified        Data.Map as Map
import qualified        Data.Set as Set
import                  Data.Hashable (Hashable)


data CoreBinds a n
 = CoreBinds
 { precomps     :: [(Name n, C.Exp    a n)]
 , streams      :: [C.Stream a n]
 , postcomps    :: [(Name n, C.Exp    a n)]
 }

programOfBinds
    :: OutputId
    -> ValType
    -> Name n
    -> Name n
    -> Name n
    -> Name n
    -> CoreBinds a n
    -> a
    -> Name n
    -> C.Program a n
programOfBinds output inpType factValName factTimeName postDate maxMapSize binds a_ret ret
 = C.Program
 { C.inputType    = inpType
 , C.factValName  = factValName
 , C.factTimeName = factTimeName
 , C.snaptimeName = postDate
 , C.maxMapSize   = maxMapSize
 , C.precomps     = precomps  binds
 , C.streams      = streams   binds
 , C.postcomps    = postcomps binds
 , C.returns      = [(output, X.XVar a_ret ret)]
 }


-- | Rip out the postcomputations into lets.
-- The result expression has the postcomputations as lets,
-- the result bindings have no postcomputations.
pullPosts :: (Hashable n, Eq n)
          => a
          -> (Pattern n, (CoreBinds a n, Name n))
          -> (CoreBinds a n, C.Exp a n)
pullPosts a (pat, (bs,ret))
 = let (pre0,post0) = deferBinds (boundOfPattern pat) (precomps bs)
       ps  = post0 <> postcomps bs
       bs' = bs { precomps = pre0, postcomps = [] }
   in  (bs', X.makeLets a ps $ X.XVar a ret)

deferBinds :: (Hashable n, Eq n) => Set.Set (Name n) -> [(Name n, C.Exp a n)] -> ([(Name n, C.Exp a n)], [(Name n, C.Exp a n)])
deferBinds defer0 binds0 = go [] [] defer0 binds0
 where
  go pres posts _ []
   = (reverse pres, reverse posts)
  go pres posts defer ((n,e):bs)
   = let b       = (n,e)
         fv      = X.freevars e
         mention = Set.intersection defer fv
     in if Set.null mention
        then go (b:pres) posts defer bs
        else go pres (b:posts) (Set.insert n defer) bs

instance Semigroup (CoreBinds a n) where
  (<>) (CoreBinds a b c) (CoreBinds d e f)
    = CoreBinds (a<>d) (b<>e) (c<>f)

instance Monoid (CoreBinds a n) where
 mempty = CoreBinds [] [] []
 mappend = (<>)

pre :: Name n -> C.Exp a n -> CoreBinds a n
pre n x = mempty { precomps = [(n,x)] }

filt :: C.Exp a n -> [C.Stream a n] -> CoreBinds a n
filt p s
 = mempty { streams = [C.SFilter p s] }

sfold :: Name n -> ValType -> C.Exp a n -> C.Exp a n -> CoreBinds a n
sfold n t z k = mempty { streams = [C.SFold n t z k] }

post :: Name n -> C.Exp a n -> CoreBinds a n
post n x = mempty { postcomps = [(n,x)] }

data ConvertError a n
 = ConvertErrorNoSuchFeature UnresolvedInputId
 | ConvertErrorPrimNoArguments               a Int Prim
 | ConvertErrorPrimAggregate                 a Prim
 | ConvertErrorGroupByHasNonGroupResult      a (Type n)
 | ConvertErrorGroupFoldNotOnGroup           a (Exp (Annot a n) n)
 | ConvertErrorContextNotAllowedInGroupBy    a (Query (Annot a n) n)
 | ConvertErrorExpNoSuchVariable             a (Name n)
 | ConvertErrorExpNestedQueryNotAllowedHere  a (Query (Annot a n) n)
 | ConvertErrorExpApplicationOfNonPrimitive  a (Exp (Annot a n) n)
 | ConvertErrorReduceAggregateBadArguments   a (Exp (Annot a n) n)
 | ConvertErrorCannotConvertType             a (Type n)
 | ConvertErrorCannotConvertTypeForPrim      a Prim (Type n)
 | ConvertErrorBadCaseNoDefault              a ValType (Exp (Annot a n) n)
 | ConvertErrorBadCaseNestedConstructors     a (Exp (Annot a n) n)
 | ConvertErrorImpossibleFold1               a
 | ConvertErrorCannotConvertAccessor         a ValType StructField
 | ConvertErrorInputTypeNotPair              a ValType
 | ConvertErrorInputTypeNotMap               a ValType
 | ConvertErrorPatternUnconvertable          a (Pattern n)
 | ConvertErrorCannotCheckKey                a (X.Exp () n C.Prim) (X.ExpError () n C.Prim)
 deriving (Show, Eq, Ord)

annotOfError :: ConvertError a n -> Maybe a
annotOfError e
 = case e of
    ConvertErrorNoSuchFeature _
     -> Nothing
    ConvertErrorPatternUnconvertable a _
     -> Just a
    ConvertErrorPrimNoArguments a _ _
     -> Just a
    ConvertErrorPrimAggregate a _
     -> Just a
    ConvertErrorGroupByHasNonGroupResult a _
     -> Just a
    ConvertErrorGroupFoldNotOnGroup a _
     -> Just a
    ConvertErrorContextNotAllowedInGroupBy a _
     -> Just a
    ConvertErrorExpNoSuchVariable a _
     -> Just a
    ConvertErrorExpNestedQueryNotAllowedHere a _
     -> Just a
    ConvertErrorExpApplicationOfNonPrimitive a _
     -> Just a
    ConvertErrorReduceAggregateBadArguments a _
     -> Just a
    ConvertErrorCannotConvertType a _
     -> Just a
    ConvertErrorCannotConvertTypeForPrim a _ _
     -> Just a
    ConvertErrorBadCaseNoDefault a _ _
     -> Just a
    ConvertErrorBadCaseNestedConstructors a _
     -> Just a
    ConvertErrorImpossibleFold1 a
     -> Just a
    ConvertErrorCannotConvertAccessor a _ _
     -> Just a
    ConvertErrorCannotCheckKey a _ _
     -> Just a
    ConvertErrorInputTypeNotPair a _
     -> Just a
    ConvertErrorInputTypeNotMap a _
     -> Just a

type ConvertM a n r
 = StateT (ConvertState n)
 (FreshT n (Either (ConvertError a n))) r

data ConvertState n
 = ConvertState
 { csInputName    :: Name n
 , csInputType    :: ValType
 , csFactTimeName :: Name n
 , csDateName     :: Name n
 , csMaxMapSize   :: Name n
 , csFeatures     :: FeatureContext () n
 , csFreshen      :: Map.Map (Name n) (Name n)

 -- | Bindings which are TemporalityElement, and will
 --   need to be packed into the buffer when doing a
 --   latest query.
 --   Unfortunately, at this time the top level uses
 --   a different mechanism (packing into the input)
 --   but it works out the same in the end.
 , csFoldRepacks  :: Map.Map (Name n) (Name n, ValType)
 }

convertInput :: ConvertM a n (Name n, ValType)
convertInput
 = ((,) <$> csInputName <*> csInputType) <$> get

convertInputName :: ConvertM a n (Name n)
convertInputName
 = csInputName <$> get

convertInputType :: ConvertM a n ValType
convertInputType
 = csInputType <$> get

convertFactTimeName :: ConvertM a n (Name n)
convertFactTimeName
 = csFactTimeName <$> get

convertDateName :: ConvertM a n (Name n)
convertDateName
 = csDateName <$> get

convertMaxMapSize :: ConvertM a n (Name n)
convertMaxMapSize
 = csMaxMapSize <$> get

convertFeatures :: ConvertM a n (FeatureContext () n)
convertFeatures
 = csFeatures <$> get

convertElementRepacks :: ConvertM a n [(Name n, Name n, ValType)]
convertElementRepacks
 = fmap (\(k, (v, cs)) -> (k,v,cs)).Map.toList . csFoldRepacks <$> get

convertAddElementRepack :: Eq n => Name n -> Name n -> ValType -> ConvertM a n ()
convertAddElementRepack from to valType
 = do   o <- get
        put (o { csFoldRepacks = Map.insert from (to, valType) (csFoldRepacks o) })
        return ()

convertWithInputName :: Name n -> ConvertM a n r -> ConvertM a n r
convertWithInputName n c
 = do   t <- convertInputType
        convertWithInput n t c

convertWithInput :: Name n -> ValType -> ConvertM a n r -> ConvertM a n r
convertWithInput n t c
 = do   o <- get
        put (o { csInputName = n, csInputType = t })
        r <- c
        put o
        return r

convertContext
        :: ConvertM a n r
        -> ConvertM a n r
convertContext with
 = do   o <- get
        r <- with
        put o
        return r

convertFreshenAdd :: (Hashable n, Eq n) => Name n -> ConvertM a n (Name n)
convertFreshenAdd prefix
 = do   n <- lift $ freshPrefixBase $ nameBase prefix
        convertFreshenAddAs prefix n
        return n

convertFreshenAddAs :: Eq n => Name n -> Name n -> ConvertM a n ()
convertFreshenAddAs from to
 = do   o <- get
        put $ o { csFreshen  = Map.insert from to $ csFreshen  o }


convertFreshenLookup :: Eq n => a -> Name n -> ConvertM a n (Name n)
convertFreshenLookup ann n
 = do   o <- get
        case Map.lookup n $ csFreshen o of
         Nothing
          -> convertError $ ConvertErrorExpNoSuchVariable ann n
         Just n'
          -> return n'

convertFreshenLookupMaybe :: Eq n => Name n -> ConvertM a n (Maybe (Name n))
convertFreshenLookupMaybe n
 = do   o <- get
        return $ Map.lookup n $ csFreshen o


convertFunctionArgType :: a -> Type n -> ConvertM a n ValType
convertFunctionArgType ann ty
 = case ty of
    TypeArrow ty' _
      -> convertValType ann ty'
    _
     -> convertError $ ConvertErrorCannotConvertType ann ty

convertValType :: a -> Type n -> ConvertM a n ValType
convertValType ann ty
 = case valTypeOfType ty of
    Nothing
     -> convertError $ ConvertErrorCannotConvertType ann ty
    Just t'
     -> return t'


convertError :: ConvertError a n -> ConvertM a n r
convertError = lift . lift . Left


windowEdge :: C.Exp () n -> WindowUnit -> C.Exp () n
windowEdge now = \case
  Days d ->
    CE.xPrim (C.PrimMinimal $ Min.PrimTime Min.PrimTimeMinusDays) CE.@~ now CE.@~ CE.constI d
  Weeks  w ->
    CE.xPrim (C.PrimMinimal $ Min.PrimTime Min.PrimTimeMinusDays) CE.@~ now CE.@~ CE.constI (w * 7)
  Months m ->
    CE.xPrim (C.PrimMinimal $ Min.PrimTime Min.PrimTimeMinusMonths) CE.@~ now CE.@~ CE.constI m


-- | These errors should only occur if
--   - there is a bug in the conversion (there is)
--   - or the program shouldn't type check
--
--   so the pretty printing doesn't have to be as good as type checking.
instance (Pretty a, Pretty n) => Pretty (ConvertError a n) where
 pretty e
  = case e of
     ConvertErrorNoSuchFeature n
      -> "No such feature: " <> pretty n

     ConvertErrorPrimNoArguments a num_args p
      -> pretty a <> ": primitive " <> pretty p <> " expects " <> pretty num_args <> " arguments but got none"

     ConvertErrorPrimAggregate a p
      -> pretty a <> ": primitive " <> pretty p <> " is an aggregate. It should have been handled earlier."

     ConvertErrorGroupByHasNonGroupResult a ut
      -> pretty a <> ": group by has wrong return type; should be a group but got " <> pretty ut

     ConvertErrorGroupFoldNotOnGroup a x
      -> pretty a <> ": group fold is not on a group; expected group but got " <> pretty x

     ConvertErrorContextNotAllowedInGroupBy a q -> pretty a <> ": only filters and aggregates are allowed in group by (the rest are TODO): " <> pretty q

     ConvertErrorExpNoSuchVariable a n
      -> pretty a <> ": no such variable " <> pretty n

     ConvertErrorExpNestedQueryNotAllowedHere a q
      -> pretty a <> ": nested query not allowed in this expression: " <> pretty q

     ConvertErrorExpApplicationOfNonPrimitive a x
      -> pretty a <> ": application of non-function: " <> pretty x

     ConvertErrorReduceAggregateBadArguments a x
      -> pretty a <> ": bad arguments to aggregate: " <> pretty x

     ConvertErrorCannotConvertType a t
      -> pretty a <> ": cannot convert base type: " <> pretty t

     ConvertErrorCannotConvertTypeForPrim a p t
      -> pretty a <> ": cannot convert prim: " <> pretty p <> "; unexpected type type: " <> pretty t

     ConvertErrorBadCaseNoDefault a t x
      -> pretty a <> ": case has no default alternative, type " <> pretty t <> line  <> pretty x

     ConvertErrorBadCaseNestedConstructors a x
      -> pretty a <> ": case has nested constructors in pattern; these should be removed by an earlier pass: " <> pretty x

     ConvertErrorImpossibleFold1 a
      -> pretty a <> ": fold1 cannot be converted; desugar first"

     ConvertErrorCannotConvertAccessor a t f
      -> pretty a <> ": field accessor cannot be converted; type: " <> pretty t <> "; field to gather: " <> pretty f

     ConvertErrorPatternUnconvertable a p
      -> pretty a <> ": pattern conversion error; desugar first:" <> pretty p

     ConvertErrorCannotCheckKey a x r
      -> pretty a <> ": cannot check type of converted key expression: " <> line <> pretty x <> line <> pretty r

     ConvertErrorInputTypeNotPair a t
      -> pretty a <> ": cannot convert, input type was expected to be a pair, got: " <> pretty t

     ConvertErrorInputTypeNotMap a t
      -> pretty a <> ": cannot convert, input type was expected to be a structure, got: " <> pretty t

