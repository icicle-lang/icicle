-- | Generate type constraints
{-# LANGUAGE DoAndIfThenElse   #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE RankNTypes        #-}
module Icicle.Source.Checker.Constraint (
    constraintsQ
  , generateQ
  , generateX
  , defaults
  ) where

import           Icicle.Source.Checker.Base
import           Icicle.Source.Checker.Error
import           Icicle.Source.Checker.Prim

import           Icicle.Source.Query
import           Icicle.Source.Type

import           Icicle.Common.Base
import qualified Icicle.Common.Fresh          as Fresh
import           Icicle.Internal.Pretty (Pretty)

import           P hiding (with)

import           Data.Hashable                (Hashable)
import           Data.List                    (unzip3, zip)
import qualified Data.Map                     as Map

import           X.Control.Monad.Trans.Either



-- | Defaulting any polymorphic Nums to Ints.
-- For example, if the query has
-- > feature salary ~> 1
-- this actually has type "forall a. Num a => a"
-- and we could safely use any number type.
--
-- Anything else can be converted to unit:
-- > feature salary ~> Right ""
-- This has type "forall a. Sum a String".
-- We could choose anything at all for the "a".
-- When converting the rest to unit, we must also descend all the way down
-- into the query.
-- All unsolved constraints bubble up, but un-defaulted types won't necessarily.
-- For example, the return type for the following is String,
-- but what is the type of the case scrutinee?
-- > case Right () | Left _ -> "left" | Right _ -> "right"
--
defaults :: (Hashable n, Eq n)
         => Query'C a n
         -> Query'C a n
defaults topq
  -- Substitute the defaults in. Num takes precedence because defaultOfAllQ would substitute numbers to Unit.
  = substTQ (Map.union defaultNums (defaultOfAllQ topq)) topq
 where
  -- Convert numeric constraints to Ints
  defaultNums
   = Map.fromList
   $ concatMap (defaultOfConstraint . snd)
   $ annConstraints
   $ annotOfQuery topq



  defaultOfConstraint (CIsNum t)
   -- It must be a type variable - if it isn't it either already has a concrete
   -- Num type such as Int, or it is a type error
   = defaultTo t IntT
  defaultOfConstraint (CPossibilityOfNum poss t)
   = defaultTo t IntT <> defaultTo poss PossibilityDefinitely
  -- Everything else should really be known by this stage.
  -- These shouldn't actually occur.
  defaultOfConstraint (CEquals _ _)
   = []
  defaultOfConstraint (CReturnOfLetTemporalities _ _ _)
   = []
  defaultOfConstraint (CDataOfLatest _ _ _ _)
   = []
  defaultOfConstraint (CPossibilityOfLatest _ _ _)
   = []
  defaultOfConstraint (CPossibilityJoin _ _ _)
   = []
  defaultOfConstraint (CTemporalityJoin _ _ _)
   = []

  defaultTo tv tr
   | TypeVar n <- tv
   = [(n, tr)]
   | otherwise
   = []

  -- Compute free *type* variables of queries and expressions
  defaultOfAllQ (Query cs x)
   = Map.unions
   ( defaultOfAllX x : fmap defaultOfAllC cs )

  defaultOfAllC c
   = let fa   = defaultOfAllA (annotOfContext c)
         fv x = fa <> defaultOfAllX x
     in case c of
          Let _ _ x -> fv x
          LetFold _ f
           -> fa                      <>
              defaultOfAllX (foldInit f) <>
              defaultOfAllX (foldWork f)
          Windowed{} -> fa
          Latest{} -> fa
          GroupBy _ x -> fv x
          GroupFold _ _ _ x -> fv x
          Distinct _ x -> fv x
          Filter _ x -> fv x

  defaultOfAllX x
   = let fa   = defaultOfAllA (annotOfExp x)
     in  case x of
           Var{} -> fa
           Nested _ q -> fa <> defaultOfAllQ q
           App _ p q -> fa <> defaultOfAllX p <> defaultOfAllX q
           Prim{} -> fa
           Case _ s pats
            ->  fa <> defaultOfAllX s <>
               (Map.unions $ fmap (defaultOfAllX . snd) pats)


  defaultOfAllA a = defaultOfAllT $ annResult a

  defaultOfAllT fullty
   = let (tmp,pos,dat) = decomposeT fullty
         fv def t = Map.fromSet (const def) (freeT t)
         tmp' = maybe Map.empty (fv TemporalityPure) tmp
         pos' = maybe Map.empty (fv PossibilityDefinitely) pos
         dat' = fv UnitT dat
     in  Map.unions [tmp', pos', dat']


-- | Generate constraints for an entire query.
--   We take the map of types of all imported functions.
constraintsQ
  :: (Hashable n, Eq n, Pretty n)
  => Map.Map (Name n) (FunctionType n)
  -> Query a n
  -> EitherT (CheckError a n) (Fresh.Fresh n) (Query'C a n)
constraintsQ env q
 = do (x, cons) <- evalGenNoLog $ top
      -- We must have been able to solve all constraints except numeric requirements.
      if   all (isNumConstraint . snd) cons
      then right x
      else hoistEither
            $ errorNoSuggestions
            $ ErrorConstraintLeftover
               (annotOfQuery q)
               (filter (not . isNumConstraint . snd) cons)
 where
  isNumConstraint CIsNum{}            = True
  isNumConstraint CPossibilityOfNum{} = True
  isNumConstraint _                   = False

  -- Perform top-level discharge of any silly leftover Possibility or Temporality joins
  top = do
   (q',_,cons) <- generateQ q env
   case dischargeCS' dischargeC'toplevel cons of
    Left errs
     -> genHoistEither
      $ errorNoSuggestions (ErrorConstraintsNotSatisfied (annotOfQuery q) errs)
    Right (sub', cons')
     -> let q'' = substTQ sub' q'
        in  return (q'', cons')


-- | Generate constraints for top-level query.
-- The Gen monad stack has a fresh name supply, as well as the state of
-- constraints and environment mapping from name to type.
--
-- The constraints flow upwards, bottom-up, but are also discharged and simplified at every level.
-- The substitutions from discharged constraints also flow upwards.
--
-- The environment mapping flows downwards, with new names only available in lexically
-- nested subexpressions and so on.
--
generateQ
  :: (Hashable n, Eq n, Pretty n)
  => Query a n
  -> GenEnv n
  -> Gen a n (Query'C a n, SubstT n, GenConstraintSet a n)

-- End of query - at this stage it is just an expression with no contexts.
generateQ (Query [] x) env
 = do   (x', s, cons) <- generateX x env
        return (Query [] x', s, cons)

-- Query with a context
generateQ qq@(Query (c:_) _) env
 -- Discharge any constraints on the result
 =   discharge annotOfQuery substTQ
 =<< case c of
    -- In the following "rules",
    --  x't stands for "temporality of x";
    --  x'p "possibility of x";
    --  x'd "data type of x"
    --  and so on.
    --
    -- >   windowed n days ~> Aggregate x'p x'd
    -- > : Aggregate x'p x'd
    Windowed _ from to
     -> do  (q', sq, t', consr) <- rest env

            -- Windowed only works for aggregates
            consT    <- requireAgg t'
            let t''   = canonT $ Temporality TemporalityAggregate t'

            let cons' = concat [consr, consT]

            let q''   = with cons' q' t'' $ \a' -> Windowed a' from to
            return (q'', sq, cons')

    -- >   latest n ~> x't x'p x'd
    -- > : Aggregate (PossibilityOfLatest x't x'p) (DataOfLatest x't x'p x'd)
    Latest _ i
     -> do  (q', sq, tq, consr) <- rest env

            let (tmpq, posq, datq)  = decomposeT tq
            retDat              <- TypeVar <$> fresh
            retPos              <- TypeVar <$> fresh

            -- Latest works for aggregates or elements.
            -- If the end is an element, such as
            --
            -- > latest 3 ~> value
            --
            -- the result type is actually an array of the last three elements.
            --
            --
            -- However, if we are in a function definition right now such as
            --
            -- > silly_function x = latest 3 ~> x
            --
            -- we do not necessarily know whether "x" is an element or an aggregate;
            -- its temporality is still just a type variable.
            --
            -- So instead of making a choice prematurely, we introduce a constraint:
            --
            -- > silly_function x = latest 3 ~> x
            -- >  : forall (x't : Temporality)
            -- >           (x'p : Possibility)
            -- >           (x'd : Data)
            -- >           (ret : Data)
            -- >    (ret = DataOfLatest x't x'p x'd)
            -- > => x't x'd -> Aggregate ret
            --
            -- The definition for DataOfLatest is in Icicle.Source.Type.Constraints,
            -- but is something like
            --
            -- > DataOfLatest Aggregate d = d
            -- > DataOfLatest Element   d = Array d
            --
            let tmpq' = fromMaybe TemporalityPure tmpq
            let posq' = fromMaybe PossibilityDefinitely posq

            let consT = require a (CDataOfLatest        retDat tmpq' posq' datq)
                      <>require a (CPossibilityOfLatest retPos tmpq' posq')

            let t'    = canonT
                      $ Temporality TemporalityAggregate
                      $ Possibility retPos retDat

            let cons' = concat [consr, consT]

            let q''   = with cons' q' t' $ \a' -> Latest a' i
            return (q'', sq, cons')

    -- >   group (Element k'p k'd) ~> Aggregate v'p v'd
    -- > : Aggregate Possibly (Group k'd v'd)
    GroupBy _ x
     -> do  (x', sx, consk)       <- generateX x env
            (q', sq, tval, consr) <- rest $ substE sx env

            let tkey = annResult $ annotOfExp x'

            consT          <-  (<>) <$> requireTemporality tkey TemporalityElement
                                    <*> requireAgg tval

            let t'  = canonT
                    $ Temporality TemporalityAggregate
                    $ Possibility PossibilityPossibly
                    $ GroupT tkey tval

            let cons' = concat [consk, consr, consT]

            let ss  = compose sx sq
            let q'' = with cons' q' t' $ \a' -> GroupBy a' x'
            return (q'', ss, cons')

    -- >   group fold (k, v) = ( |- Q : Aggregate g'p (Group a'k a'v))
    -- >   ~> (k: Element a'k, v: Element a'v |- Aggregate a'p a)
    -- >    : Aggregate (PossibilityJoin g'p a'p) a
    GroupFold ann k v x
     -> do  (x', sx, consg) <- generateX x env
            let tgroup       = annResult $ annotOfExp x'

            retk <- Temporality TemporalityElement <$> patTy ann k
            retv <- Temporality TemporalityElement <$> patTy ann v

            let env' = removeElementBinds $ substE sx env
            (q', sq, t', consr)
                <- rest =<< goPat ann k retk =<< goPat ann v retv env'

            consT  <-  requireAgg  t'
            consgt <-  requireAgg  tgroup
            let consgd = requireData tgroup (GroupT retk retv)

            (poss, consp)  <- requirePossibilityJoin tgroup t'

            let t'' = canonT
                    $ Temporality TemporalityAggregate
                    $ Possibility poss t'

            let cons' = concat [consg, consr, consT, consgt, consgd, consp]

            let ss  = compose sx sq
            let q'' = with cons' q' t'' $ \a' -> GroupFold a' k v x'
            return (q'', ss, cons')

    -- >   distinct (Element k'p k'd) ~> Aggregate v'p v'd
    -- > : Aggregate Possibly v'd
    Distinct _ x
     -> do  (x', sx, consk)     <- generateX x env
            (q', sq, t', consr) <- rest $ substE sx env

            let tkey = annResult $ annotOfExp x'

            consT          <-  (<>) <$> requireTemporality tkey TemporalityElement
                                    <*> requireAgg t'

            let t'' = canonT
                    $ Temporality TemporalityAggregate
                    $ Possibility PossibilityPossibly t'

            let cons' = concat [consk, consr, consT]

            let ss  = compose sx sq
            let q'' = with cons' q' t'' $ \a' -> Distinct a' x'
            return (q'', ss, cons')

    -- >   filter (Element p'p Bool) ~> Aggregate x'p x'd
    -- > : Aggregate (PossibilityJoin p'p x'p) x'd
    Filter _ x
     -> do  (x', sx, consx)     <- generateX x env
            (q', sq, t', consr) <- rest $ substE sx env

            let pred = annResult $ annotOfExp x'

            consT          <-  (<>) <$> requireTemporality pred TemporalityElement
                                    <*> requireAgg t'
            let consd       = requireData pred BoolT
            (poss, consp)  <-  requirePossibilityJoin pred t'

            let t'' = canonT
                    $ Temporality TemporalityAggregate
                    $ Possibility poss t'

            let cons' = concat [consx, consr, consT, consd, consp]

            let ss  = compose sx sq
            let q'' = with cons' q' t'' $ \a' -> Filter a' x'
            return (q'', ss, cons')

    -- >     let fold  bind = ( |- Pure z'p a'd) : ( bind : Element z'p a'd |- Element k'p a'd)
    -- >  ~> (bind : Aggregate k'p a'd |- Aggregate r'p r'd)
    -- >   :  Aggregate r'p r'd
    --
    -- >     let fold1 bind = ( |- Element z'p a'd) : ( bind : Element z'p a'd |- Element k'p a'd)
    -- >  ~> (bind : Aggregate Possibly a'd |- Aggregate r'p r'd)
    -- >   :  Aggregate r'p r'd
    LetFold ann f
     -> do  (i,si, csi) <- generateX (foldInit f) env
            iniPos   <- TypeVar <$> fresh
            let ip  = getPossibilityOrDefinitely $ annResult $ annotOfExp i
                ip' = if ip == PossibilityDefinitely then iniPos else ip
                ti  = canonT
                    $ Temporality TemporalityElement
                    $ Possibility ip'
                    $ annResult $ annotOfExp i

            let env' = substE si env
            (w,sw, csw) <- generateX (foldWork f) =<< goPat ann (foldBind f) ti env'

            let bindType
                 | FoldTypeFoldl1 <- foldType f
                 = canonT
                 $ Temporality TemporalityAggregate
                 $ Possibility PossibilityPossibly
                 $ annResult $ annotOfExp w

                 | otherwise
                 = canonT
                 $ Temporality TemporalityAggregate
                 $ annResult $ annotOfExp w

            let env'' = substE sw env'
            (q', sq, t', consr) <- rest =<< goPat ann (foldBind f) bindType env''

            consf
              <- case foldType f of
                  FoldTypeFoldl1
                   -> requireTemporality (annResult $ annotOfExp i) TemporalityElement
                  FoldTypeFoldl
                   -> requireTemporality (annResult $ annotOfExp i) TemporalityPure

            let (_, _,it) = decomposeT $ annResult $ annotOfExp i
            let (_,wp,wt) = decomposeT $ annResult $ annotOfExp w
            let wp'       = maybe PossibilityDefinitely id wp

            consT <-  (<>) <$> requireAgg t'
                           <*> requireTemporality (annResult $ annotOfExp w) TemporalityElement
            let conseq = concat
                       [ require a (CEquals it wt)
                       , require a (CPossibilityJoin iniPos wp' ip') ]

            let cons' = concat [csi, csw, consr, consf, consT, conseq]

            let t'' = canonT $ Temporality TemporalityAggregate t'
            let s'  = si `compose` sw `compose` sq

            let q'' = with cons' q' t'' $ \a' -> LetFold a' (f { foldInit = i, foldWork = w })
            return (q'', s', cons')

    -- Lets are not allowed if it means the binding would not be able to be used in the body.
    -- For example, trying to bind an Aggregate where the body is an Element, means that the
    -- definition cannot possibly be used: any reference to the binding would force the body to be
    -- an Aggregate itself.
    --
    -- The constraint "let't = ReturnOfLetTemporalities def't body't" is used for this.
    --
    -- >   let n = ( |- def't def'p def'd )
    -- >    ~> ( n : def't def'p def'd |- body't body'p body'd )
    -- > : (ReturnOfLetTemporalities def't body't) body'p body'd
    Let ann n x
     -> do  (x', sx, consd)  <- generateX x env
            let x'typ = annResult $ annotOfExp x'

            (q',sq,tq,consr) <- rest =<< goPat ann n x'typ (substE sx env)

            retTmp   <- TypeVar <$> fresh
            let tmpx  = getTemporalityOrPure x'typ
            let tmpq  = getTemporalityOrPure $ tq

            let consT = require a (CReturnOfLetTemporalities retTmp tmpx tmpq)
            let cons' = concat [consd, consr, consT]

            let t'    = canonT $ Temporality retTmp tq

            let ss  = compose sx sq
            let q'' = with cons' q' t' $ \a' -> Let a' n x'
            return (q'', ss, cons')

 where
  -- Create the binding environment for a pattern.
  -- We are only permitting total patterns, so PairT
  -- variables and non-binders.
  goPat _ PatDefault _ e
    -- A Default _ binding doesn't effect the rest
    -- of the program.
    = return e
  goPat _ (PatVariable n) typ e
    -- A binding variable is accessible downstream
    -- so bind it and its type to the environment.
    = return $ bindT n typ e
  goPat ann (PatCon ConTuple [a'pat,b'pat]) typ e
    -- As we can put a let at any temporality and
    -- possibility, we need to decompose the pattern
    -- and keep the possibility/temporality of the
    -- values of the pair the same as the pair being
    -- bound.
    | (mt,mp,canon'typ) <- decomposeT $ canonT typ
    , PairT a'typ b'typ <- canon'typ
    = do
      a'env <- goPat ann a'pat (recomposeT (mt, mp, a'typ)) e
      b'env <- goPat ann b'pat (recomposeT (mt, mp, b'typ)) a'env
      return b'env
  goPat ann (PatCon ConTuple _) btyp _
    -- It's a type error, as we can't bind a non-pair
    -- pattern to a pair. We don't have type variables
    -- for the what the elements of the pair should be,
    -- so draw some fresh ones.
    = do
      p1 <- TypeVar <$> fresh
      p2 <- TypeVar <$> fresh
      genHoistEither
        $ errorNoSuggestions
        $ ErrorConstraintsNotSatisfied ann
          [(ann, CannotUnify (PairT p1 p2) btyp)]
  goPat ann pat _ _
    -- It's not a pair, default or variable. All other
    -- patterns are Partial, so we disallow it.
    = genHoistEither
    $ errorNoSuggestions (ErrorPartialBinding ann pat)

  -- Generate a fresh set of type variables which a
  -- total pattern must match. This flow upwards into
  -- a group fold.
  patTy _ PatDefault
    = TypeVar <$> fresh
  patTy _ PatVariable {}
    = TypeVar <$> fresh
  patTy ann (PatCon ConTuple [a'pat,b'pat])
    = do a'typ <- patTy ann a'pat
         b'typ <- patTy ann b'pat
         return (PairT a'typ b'typ)
  patTy ann pat
    -- It's not a pair, default or variable. All other
    -- patterns are Partial, so we disallow it.
    = genHoistEither
    $ errorNoSuggestions (ErrorPartialBinding ann pat)


  a  = annotOfContext c

  -- Generate constraints for the remainder of the query, and rip out the result type
  rest e
   = do (q',s',cx') <- generateQ (qq { contexts = drop 1 $ contexts qq }) e
        return (q', s', annResult $ annotOfQuery q',cx')

  -- Rebuild the result with given substitution and type, using the given constraints
  with cs q' t' c'
   = let a' = Annot a t' cs
     in  q' { contexts = c' a' : contexts q' }

  -- Helpers for adding constraints
  requireTemporality ty tmp
   | (tmp',_,_) <- decomposeT ty
   = case tmp' of
      Just TemporalityPure -> return []
      Nothing              -> return []
      Just tmp''
       -> return $ require a (CEquals tmp'' tmp)

  requireAgg t
   = requireTemporality t TemporalityAggregate

  requireData t1 t2
   = let (_,_,d1) = decomposeT t1
         (_,_,d2) = decomposeT t2
     in  require a (CEquals d1 d2)

  requirePossibilityJoin t1 t2
   = do poss   <- TypeVar <$> fresh
        let pt1 = getPossibilityOrDefinitely t1
        let pt2 = getPossibilityOrDefinitely t2
        let c'  = require a (CPossibilityJoin poss pt1 pt2)
        return (poss, c')

-- | Generate constraints for expression
generateX
  :: (Hashable n, Eq n, Pretty n)
  => Exp a n
  -> GenEnv n
  -> Gen a n (Exp'C a n, SubstT n, GenConstraintSet a n)
generateX x env
 =   discharge annotOfExp substTX
 =<< case x of
    -- Variables can only be values, not functions.
    Var a n
     -> do (fErr, argsT, resT, cons') <- lookup a n env

           when (not $ null argsT)
             $ genHoistEither
             $ errorNoSuggestions (ErrorFunctionWrongArgs a x fErr [])

           let x' = annotate cons' resT
                  $ \a' -> Var a' n
           return (x', Map.empty, cons')

    -- Nested just has the type of its inner query.
    Nested _ q
     -> do (q', sq, cons') <- generateQ q env

           let x' = annotate cons' (annResult $ annotOfQuery q')
                  $ \a' -> Nested a' q'
           return (x', sq, cons')

    -- Applications are a bit more complicated.
    -- If your function expects an argument with a particular temporality,
    -- and you give it that temporality, it is fine; function application is as usual.
    -- (This is even if it expects Pure, and you apply a Pure)
    --
    -- If your function expects a Pure argument, and you give it another temporality
    -- such as Element, that means the argument must be implicitly unboxed to a pure computation,
    -- and then the result of application being reboxed as an Element.
    -- Reboxing can only work if the result type is pure or the desired temporality;
    -- you could not rebox an Aggregate result to an Element.
    --
    -- Look at an application of (+), for example:
    --  (+) : Pure Int -> Pure Int -> Pure Int
    -- However if one of its arguments is an Element:
    --  (x : Pure Int) + (y : Element Int) : Element Int
    -- here the y is unboxed, then the result is reboxed.
    --
    App a _ _
     -> let (f, args)   = takeApps x
            look        | Prim _ p <- f
                        = primLookup a p
                        | Var _ n  <- f
                        = lookup a n env
                        | otherwise
                        = genHoistEither
                        $ errorNoSuggestions (ErrorApplicationNotFunction a x)
            genXs [] _  = return []
            genXs (xx:xs) env'
                        = do (xx',s,c) <- generateX xx env'
                             rs       <- genXs xs (substE s env')
                             return ((xx',s,c) : rs)

        in do   (fErr, argsT, resT, consf) <- look

                (args', subs', consxs)     <- unzip3 <$> genXs args env
                let argsT'                  = fmap (annResult.annotOfExp) args'

                when (length argsT /= length args)
                 $ genHoistEither
                 $ errorNoSuggestions (ErrorFunctionWrongArgs a x fErr argsT')

                let go (t, c) u     = appType a t u c
                (resT', consap) <- foldM go (resT, []) (argsT `zip` argsT')

                let s' = foldl compose Map.empty subs'
                let cons' = concat (consf : consap : consxs)

                let f' = annotate cons' resT' $ \a' -> reannotX (const a') f

                return (foldl mkApp f' args', s', cons')

    -- Unapplied primitives should be relatively easy
    Prim a p
     -> do (fErr, argsT, resT, cons') <- primLookup a p

           when (not $ null argsT)
             $ genHoistEither
             $ errorNoSuggestions (ErrorFunctionWrongArgs a x fErr [])

           let x' = annotate cons' resT
                  $ \a' -> Prim a' p
           return (x', Map.empty, cons')

    -- Cases require:
    --
    --  1. Alternatives to have "join-able" types, i.e. no mixing of aggregates and elements.
    --  2. The return temporality is the join of the scrutinee with the alternatives.
    --  3. The type of the scrutinee is compatible with all patterns.
    --
    Case a scrut pats
     -> do (scrut', sub, consS) <- generateX scrut env

           -- Destruct the scrutinee type into the base type
           -- and the temporality (defaulting to Pure).
           let scrutT  = annResult $ annotOfExp     scrut'
           let scrutTm = getTemporalityOrPure       scrutT
           let scrutPs = getPossibilityOrDefinitely scrutT

           -- Require the scrutinee and the alternatives to have compatible temporalities.
           returnType  <- TypeVar <$> fresh
           returnTemp  <- TypeVar <$> fresh
           returnTemp' <- TypeVar <$> fresh
           let consTj  =  require a (CTemporalityJoin returnTemp' scrutTm returnTemp)

           returnPoss  <- TypeVar <$> fresh
           returnPoss' <- TypeVar <$> fresh
           let consPs  =  require a (CPossibilityJoin returnPoss' scrutPs returnPoss)

           (pats', subs, consA) <- generateP a scrutT returnType returnTemp' returnTemp returnPoss pats (substE sub env)

           let t'    = canonT
                     $ Temporality returnTemp'
                     $ Possibility returnPoss' returnType
           let subs' = compose sub subs
           let cons' = concat [consS, consTj, consPs, consA]

           let x' = annotate cons' t'
                  $ \a' -> Case a' scrut' pats'

           return (x', subs', cons')
  where
  annotate cs t' f
   = let a' = Annot (annotOfExp x) t' cs
     in  f a'


generateP
  :: (Hashable n, Eq n, Pretty n)
  => a
  -> Type n                 -- ^ scrutinee type
  -> Type n                 -- ^ result base type
  -> Type n                 -- ^ top-level result temporality
  -> Type n                 -- ^ chained result temporality of previous alternative
  -> Type n                 -- ^ result possibility
  -> [(Pattern n, Exp a n)] -- ^ pattern and alternative
  -> GenEnv n
  -> Gen a n ([(Pattern n, Exp'C a n)], SubstT n, GenConstraintSet a n)

generateP ann _ _ _ resTm resPs [] _
 = do   let consT = require ann (CEquals resTm TemporalityPure)
        let consP = require ann (CEquals resPs PossibilityDefinitely)
        return ([], Map.empty, concat [consT, consP])

generateP ann scrutTy resTy resTmTop resTm resPs ((pat, alt):rest) env
 = do   (t, envp) <- goPat pat env

        let (_,_,datS) = decomposeT $ canonT scrutTy
        let conss      = require (annotOfExp alt) (CEquals datS t)

        (alt', sub, consa) <- generateX alt envp

        let altTy' = annResult (annotOfExp alt')
        let altTp  = getTemporalityOrPure       altTy'
        let altPs  = getPossibilityOrDefinitely altTy'
        resTp'     <- TypeVar <$> fresh
        resPs'     <- TypeVar <$> fresh

        -- Require alternative types to have the same temporality if they
        -- do have temporalities. Otherwise defaults to TemporalityPure.
        let consT = concat
                  -- Require the alternative types without temporality to be the same.
                  [ requireData resTy altTy'
        -- Require return temporality to be compatible with alternative temporalities.
                  , require (annotOfExp alt) (CTemporalityJoin resTm resTp' altTp)
                  , require (annotOfExp alt) (CPossibilityJoin resPs resPs' altPs)
                  ]

        (rest', subs, consr) <- generateP ann scrutTy resTy resTmTop resTp' resPs' rest (substE sub env)
        let cons'       = concat [conss, consa, consT, consr]
        let alt''       = substTX subs alt'
        let subs'       = compose sub subs
        let patalts     = (pat, alt'') : rest'
        return (patalts, subs', cons')

 where
  requireData t1 t2
   = let (_,_,d1) = decomposeT t1
         (_,_,d2) = decomposeT t2
     in  require ann $ CEquals d1 d2

  goPat  PatDefault e
   = (,e) . TypeVar <$> fresh

  goPat (PatVariable n) e
   = do datV              <- TypeVar <$> fresh
        -- The bound variable is actually Definite, because the case will only succeed
        -- if the scrutinee is an actual value.
        --
        -- CASEHACK: pattern bindings have the temporality of the whole case.
        -- This is to work around a flaw in the conversion to Core.
        -- We cannot convert this:
        -- > case (Left 0)
        -- >  | Left l  -> fold a = l : l * 2 ~> a
        -- >  | Right r -> fold b = r : r / 2 ~> b
        -- > end
        -- Because conversion would require a case at each step: in the zero of the fold,
        -- and in the kons of the fold, as well as in pre and eject.
        -- So we outlaw this, by making sure the pattern binding has temporality of the
        -- return: which means this example is ill-typed, as the binding (l :: Aggregate _)
        -- could only be used at the end of the fold.
        let env'           = bindT n (recomposeT (Just resTmTop, Nothing, datV)) e
        return (datV, env')

  goPat (PatCon ConSome  [p]) e
   = fmap (first OptionT) $ goPat p e
  goPat (PatCon ConNone  []) e
   = (,e) . OptionT . TypeVar <$> fresh
  goPat (PatCon ConTrue  []) e
   = return (BoolT, e)
  goPat (PatCon ConFalse []) e
   = return (BoolT, e)
  goPat (PatCon ConTuple [a,b]) e
   = do (ta, ea) <- goPat a e
        (tb, eb) <- goPat b ea
        return (PairT ta tb, eb)
  goPat (PatCon (ConError _)  []) e
   = return (ErrorT, e)

  goPat (PatCon ConLeft  [p]) e
   = do (l,e') <- goPat p e
        r      <- TypeVar <$> fresh
        return ( SumT l r , e' )
  goPat (PatCon ConRight  [p]) e
   = do l      <- TypeVar <$> fresh
        (r,e') <- goPat p e
        return ( SumT l r , e' )

  goPat _ _
   = genHoistEither
   $ errorNoSuggestions (ErrorCaseBadPattern (annotOfExp alt) pat)


appType
 :: (Hashable n)
 => a
 -> Type n
 -> (Type n, Type n)
 -> GenConstraintSet a n
 -> Gen a n (Type n, GenConstraintSet a n)
appType ann resT (expT,actT) cons = do
  let (tmpE,posE,datE) = decomposeT $ canonT expT
  let (tmpA,posA,datA) = decomposeT $ canonT actT
  let (tmpR,posR,datR) = decomposeT $ canonT resT
  let consD            = require ann (CEquals datE datA)

  (tmpR', consT) <- checkTemp (purely tmpE) (purely tmpA) (purely tmpR)
  (posR', consP) <- checkPoss (definitely posE) (definitely posA) (definitely posR)

  let t = recomposeT (tmpR', posR', datR)
  return (t, concat [cons, consD, consT, consP])

 where
  checkTemp = check' TemporalityPure       CTemporalityJoin
  checkPoss = check' PossibilityDefinitely CPossibilityJoin

  check' pureMode joinMode modE modA modR
   | Nothing <- modA
   = return (modR, [])
   | Just _  <- modA
   , Nothing <- modE
   , Nothing <- modR
   = return (modA, [])
   | Just a' <- modA
   , Nothing <- modE
   , Just r' <- modR
   = do r'' <- TypeVar <$> fresh
        let j = joinMode r'' a' r'
        return (Just r'', require ann j)
   -- Just <- modA
   -- Just <- modE
   -- ?    <- modR
   | otherwise
   = do ignore <- TypeVar <$> fresh
        let j = joinMode ignore (maybe pureMode id modE) (maybe pureMode id modA)
        return (modR, require ann j)


  purely (Just TemporalityPure) = Nothing
  purely tmp = tmp

  definitely (Just PossibilityDefinitely) = Nothing
  definitely pos = pos


