-- | Check invariants of a query
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Source.Checker.Invariants (
    invariantQ
  , invariantX
  ) where
import                  Icicle.Common.Base
import                  Icicle.Source.Checker.Base
import                  Icicle.Source.Checker.Error
import                  Icicle.Source.Query

import                  P


import                  Data.List (zip)
import                  Data.Hashable (Hashable)
import qualified        Data.Map as Map



type Result a n = Either (CheckError a n) ()

invariantQ
        :: (Hashable n, Eq n)
        => CheckEnv a n
        -> Query    a n
        -> Result   a n
invariantQ ctx (Query [] x)
 = invariantX ctx x

invariantQ ctx (Query (c:cs) xfinal)
 = case c of
    Windowed{}
     | allowWindows inv
     -> go
     | otherwise
     -> errBanWindow


    Latest{}
     | allowLatest inv
     -> go
     | otherwise
     -> errBanLatest

    GroupBy _ x
     -> goX x >> go

    Distinct _ x
     -> goX x >> go

    ArrayFold _ _ x
     | allowGroupFolds inv
     -> goX x >> goBanAll
     | otherwise
     -> errBanGroupFold

    GroupFold _ _ _ x
     | allowGroupFolds inv
     -> goX x >> goBanAll
     | otherwise
     -> errBanGroupFold

    Filter _ x
     -> goX x >> go

    FilterLet _ _ x
     -> goX x >> go
    LetFold _ f
     -> goF (foldInit f) >> goF (foldWork f) >> go
    Let _ _ x
     -> goF x >> go
    LetScan _ _ x
     | allowLetScan inv
     -> goX x >> go
     | otherwise
     -> errBanLetScan

 where
  inv = checkInvariants ctx
  q' = Query cs xfinal
  go = invariantQ ctx q'
  goX = invariantX ctx
  goF = invariantX ctx { checkInvariants = inv { allowLetScan = False }}

  goBanAll
     = flip invariantQ q'
     $ ctx { checkInvariants = inv { allowLatest = False
                                   , allowWindows = False
                                   , allowGroupFolds = False }}
  errBanLatest
   = errorSuggestions
      (ErrorContextNotAllowedHere (annotOfContext c) c)
      [ Suggest "Latest is not allowed inside group-folds."
      , Suggest "Group folds aren't ordered with respect to time, so `latest` doesn't make sense."
      , Suggest "Note that 'newest' is implemented using latest, so you might see this error even without an explicit `latest`."]

  errBanWindow
   = errorSuggestions
      (ErrorContextNotAllowedHere (annotOfContext c) c)
      [ Suggest "Windows cannot be inside group-folds."
      , Suggest "Group folds results don't carry time with them, so a window doesn't make sense here."]

  errBanGroupFold
   = errorSuggestions
      (ErrorContextNotAllowedHere (annotOfContext c) c)
      [ Suggest "Group folds are unsupported inside other group folds." ]

  errBanLetScan
   = errorSuggestions
      (ErrorContextNotAllowedHere (annotOfContext c) c)
      [ Suggest "Running aggregations (let scan) are unsupported inside group, nested let bindings, and fold worker functions"
      , Suggest "For folds and lets, try pulling the let scan to the top of the query." ]


invariantX
        :: (Hashable n, Eq n)
        => CheckEnv a n
        -> Exp      a n
        -> Result   a n
invariantX ctx x
 = case x of
    Var a n
     -> goFun a n []
    Lam a n q
     -> goFun a n [q]
    Nested _ q
     -> invariantQ ctx q
    App{}
     | (f,xs)  <- takeApps x
     , Var a n <- f
     -> goFun a n xs
    App _ p q
     -> invariantX ctx p >> invariantX ctx q
    Prim{}
     -> return ()
    Case _ s ps
     -> invariantX ctx s >> mapM_ (invariantX ctx . snd) ps
    If _ s t f
     -> invariantX ctx s >> invariantX ctx t >> invariantX ctx f
    Access _ e _
     -> invariantX ctx e
    Record _ fs
     -> mapM_ (mapM_ (invariantX ctx)) fs


 where
  goFun a n args
   | Just fun <- Map.lookup n $ checkBodies ctx
   = let ctx' = foldl bindArg ctx (argumentsX fun `zip` args)
     in  errorInFunctionEither a n
       $ invariantX ctx' fun
   | otherwise
   = mapM_ (invariantX ctx) args

  bindArg ctx' ((_,n),def)
   = ctx'
   { checkBodies = Map.insert n def
                 $ checkBodies ctx'
   }

  argumentsX :: Exp a n -> [(a, Name n)]
  argumentsX (Lam a n p) =
    (a, n) : argumentsX p
  argumentsX _ = []
