-- | Basic things for basic people
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE PatternGuards              #-}
{-# LANGUAGE OverloadedStrings          #-}
module Icicle.Source.Checker.Base (
    CheckEnv (..)
  , Invariants (..)
  , emptyCheckEnv
  , emptyInvariants

  , CheckOptions (..)
  , optionBigData
  , optionSmallData
  , defaultCheckOptions

  , GenEnv, GenConstraintSet
  , Gen(..)
  , Query'C
  , Exp'C
  , evalGen
  , evalGenNoLog
  , genHoistEither

  , CheckLog(..)
  , DischargeInfo(..)
  , checkLog

  , require
  , discharge
  , fresh
  , introForalls
  , lookup
  , bindT
  , withBind
  , purifyUsableBinds
  , removeAggregateBinds

  , substE
  , substTQ
  , substTX
  , substAnnot

  ) where

import           Icicle.Source.Checker.Error

import           Icicle.Source.Query
import           Icicle.Source.Type

import           Icicle.Common.Base
import qualified Icicle.Common.Fresh         as Fresh
import           Icicle.Internal.Pretty

import           P

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.State.Lazy

import qualified Data.Map                    as Map
import           Data.Hashable (Hashable)


-- | Type checking environment.
-- Keep track of all the things we need to know
data CheckEnv a n
 = CheckEnv {
 -- | Mapping from variable names to whole types
   checkEnvironment :: Map.Map (Name n) (Scheme n)
 -- | Function bodies
 , checkBodies      :: Map.Map (Name n) (Exp a n)
 , checkInvariants  :: Invariants
 }

-- | Typechecking invariants that aren't checked by the type system.
--   i.e. unimplemented things.
--
data Invariants
 = Invariants {
   allowLatest :: Bool
 , allowWindows :: Bool
 , allowGroupFolds :: Bool
 -- | Unimplemented in Core: let-scans inside folds, groups, lets
 , allowLetScans   :: Bool
 }

-- | Initial environment at top-level, not inside a group, and allowing contexts
emptyCheckEnv :: CheckEnv a n
emptyCheckEnv
 = CheckEnv Map.empty Map.empty emptyInvariants

emptyInvariants :: Invariants
emptyInvariants = Invariants True True True True

--------------------------------------------------------------------------------


data CheckOptions
 = CheckOptions
 { checkOptionRequireResumable :: Bool
 , checkOptionNowPure          :: Bool
 } deriving (Show, Eq, Ord)

optionBigData :: CheckOptions
optionBigData = CheckOptions True True

optionSmallData :: CheckOptions
optionSmallData = CheckOptions False True

defaultCheckOptions :: CheckOptions
defaultCheckOptions = optionSmallData


--------------------------------------------------------------------------------


type GenEnv n             = Map.Map (Name n) (Scheme n)
type GenConstraintSet a n = [(a, Constraint n)]

data DischargeInfo a n = DischargeInfo
  { dischargeType :: Type n
  , dischargeConstraints :: GenConstraintSet a n
  , dischargeSubst :: SubstT n
  }
 deriving Show

instance (Pretty n) => Pretty (DischargeInfo a n) where
 pretty (DischargeInfo t cs s)
  = vsep ([ty] <> cons' <> subs')
  where
   ty = "T: " <> indent 0 (pretty t)
   cons'
    | null cs
    = []
    | otherwise
    = [cons]
   subs'
    | null s
    = []
    | otherwise
    = [subs]

   cons = "C: " <> indent 0 (vsep $ fmap (pretty . snd) cs)
   subs = "S: " <> indent 0 (vsep $ fmap (\(k,v) -> pretty k <> " = " <> pretty v) $ Map.toList s)


data CheckLog a n
 = CheckLogDischargeOk Doc (DischargeInfo a n) (DischargeInfo a n)
 | CheckLogDischargeError Doc (DischargeInfo a n) [(a,DischargeError n)]
 deriving Show

instance (Pretty a, Pretty n) => Pretty (CheckLog a n) where
 pretty (CheckLogDischargeOk e i0 i1)
  | emptyi i0 && emptyi i1
  = "visit     " <> indent 0 (pretty e <> " : " <> pretty (dischargeType i0))
  | otherwise
  = "discharge " <> indent 0 (pretty e) <> line <>
    "  before: " <> indent 0 (pretty i0) <> line <>
    "  after:  " <> indent 0 (pretty i1)
  where
   emptyi (DischargeInfo _ cs s) = null cs && null s
 pretty (CheckLogDischargeError s i0 errs)
  = "discharge " <> indent 0 (pretty s) <> line <>
    "  before: " <> indent 0 (pretty i0) <> line <>
    "  errors: " <> indent 0 (pretty errs)


newtype Gen a n t
 = Gen { constraintGen :: EitherT (CheckError a n) (StateT [CheckLog a n] (Fresh.Fresh n)) t }
 deriving (Functor, Applicative, Monad)

evalGen
    :: Gen a n t
    -> (Fresh.Fresh n) (Either (CheckError a n) t, [CheckLog a n])
evalGen f
 = runStateT (runEitherT $ constraintGen f) []

evalGenNoLog
    :: Gen a n t
    -> EitherT (CheckError a n) (Fresh.Fresh n) t
evalGenNoLog f = newEitherT (fst <$> evalGen f)

checkLog :: CheckLog a n -> Gen a n ()
checkLog l = Gen . lift $ modify (l:)

genHoistEither :: Either (CheckError a n) t -> Gen a n t
genHoistEither = Gen . hoistEither

type Query'C a n = Query (Annot a n) n
type Exp'C   a n = Exp   (Annot a n) n


-- | Add a constraint to the context.
require :: a -> Constraint n -> GenConstraintSet a n
require a c = [(a,c)]

-- | Discharge the constraints in some context after applying some type substitutions.
--
discharge
  :: (Hashable n, Eq n, Pretty q)
  => (q -> Annot a n)
  -> (SubstT n -> q -> q)
  -> (q, SubstT n, GenConstraintSet a n)
  -> Gen a n (q, SubstT n, GenConstraintSet a n)
discharge annotOf sub (q, s, conset)
 = do let annot     = annotOf q
      let log_ppr   = pretty q
      let log_info0 = DischargeInfo (annResult annot) conset s
      let cs = nubConstraints $ fmap (\(a,c) -> (a, substC s c)) conset

      case dischargeCS cs of
       Left errs -> do
        checkLog (CheckLogDischargeError log_ppr log_info0 errs)
        genHoistEither $ errorNoSuggestions (ErrorConstraintsNotSatisfied (annAnnot annot) errs)
       Right (s', cs') -> do
        let s'' = compose s s'
        let q'  = sub s'' q
        let annot' = annotOf q'
        let log_info1 = DischargeInfo (annResult annot') cs' s''
        checkLog (CheckLogDischargeOk log_ppr log_info0 log_info1)
        return (q', s'', cs')

fresh :: Hashable n => Gen a n (Name n)
fresh
 = Gen . lift . lift $ Fresh.fresh

-- | Freshen type scheme by applying introduction rules to foralls.
--
--   We only support rank 1 polymorphism, for example
--
--   > forall a b. a -> b -> a
--
introForalls
  :: (Hashable n, Eq n)
  => a
  -> Scheme n
  -> Gen a n (Scheme n, Type n, GenConstraintSet a n)
introForalls ann f
 = case f of
    Forall ns cs x -> do
      freshen <- Map.fromList <$> mapM mkSubst ns
      let cons = concatMap (require ann . substC freshen) cs
      let sub  = substT freshen x
      return (f, sub, cons)

 where
  mkSubst n
   = ((,) n . TypeVar) <$> fresh

-- | Look up a name in the context. Return the original type schem, along with the
--   type where all forall-quantified variables have been freshen'd.
lookup
  :: (Hashable n, Eq n)
  => a
  -> Name n
  -> GenEnv n
  -> Gen a n (Scheme n, Type n, GenConstraintSet a n)
lookup ann n env
 = case Map.lookup n env of
     Just t
      -> introForalls ann t
     Nothing
      -> genHoistEither
       $ errorSuggestions (ErrorNoSuchVariable ann n)
                           [AvailableBindings n $ Map.toList env]

-- | Bind a new to a type in the given context.
bindT :: Eq n => Name n -> Type n -> GenEnv n -> GenEnv n
bindT n t
 = Map.insert n (function0 t)

-- | Temporarily add the binding to a context, then do something.
withBind
  :: Eq n
  => Name n
  -> Type n
  -> GenEnv n
  -> (GenEnv n -> Gen a n r)
  -> Gen a n r
withBind n t old gen
 = gen (bindT n t old)

-- | For map and array folds.
--   Group folds can use bindings which are in the same phase
--   as them, so a group fold over an aggregate can use aggregates
--   computed previously; but they can use them in any position,
--   so what we do is rewrite terms with matching temporality as
--   pure binds, and remove non-matching terms.
purifyUsableBinds :: Eq n => Type n -> GenEnv n -> GenEnv n
purifyUsableBinds typ =
  Map.mapMaybe purifyElements
 where
  purifyElements ft =
    case getTemporalityOrPure (schemeType ft) of
      TemporalityPure ->
        Just ft
      other | typ == other ->
        Just $ ft { schemeType = canonT (Temporality TemporalityPure (schemeType ft)) }
      _ ->
        Nothing


-- | For Let Scans
--   Let scans reify an aggregation back into an element, but have to actually
--   produce the aggregation themselves due to how the core is elaborated.
--   So inside the let scan binding, we can't allow the use of pre-existing
--   aggregations.
removeAggregateBinds :: Eq n => GenEnv n -> GenEnv n
removeAggregateBinds =
  Map.map go
    where

  go ft =
    let
      t =
        getTemporalityOrPure (schemeType ft)
    in
      ft {
        schemeConstraints =
          CTemporalityJoin TemporalityElement TemporalityElement t :
            schemeConstraints ft
      }


substE :: Eq n => SubstT n -> GenEnv n -> GenEnv n
substE s
 = fmap (substF s)

substTQ :: (Hashable n, Eq n) => SubstT n -> Query'C a n -> Query'C a n
substTQ s
 = reannot (substAnnot s)

substTX :: (Hashable n, Eq n) => SubstT n -> Exp'C a n -> Exp'C a n
substTX s
 = reannot (substAnnot s)

substAnnot :: (Hashable n, Eq n) => SubstT n -> Annot a n -> Annot a n
substAnnot s ann
 = ann
 { annResult = substT s $ annResult ann
 , annConstraints = nubConstraints $ fmap (\(a,c) -> (a, substC s c)) $ annConstraints ann
 }
