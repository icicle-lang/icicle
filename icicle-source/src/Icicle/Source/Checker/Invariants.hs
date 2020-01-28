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
     -> goBanWindow
     | otherwise
     -> errBanWindow "Consider moving the window to the start of the query."

    Latest{}
     -> goBanAll

    GroupBy _ x
     -> goX x >> goBanWindowAndGroupFold

    Distinct _ x
     -> goX x >> goBanWindowAndGroupFold

    GroupFold _ _ _ x
     | allowGroupFolds inv
     -> goX x >> goBanAll
     | otherwise
     -> errBanGroupFold

    Filter _ x
     -> goX x >> go
    LetFold _ f
     -> goX (foldInit f) >> goX (foldWork f) >> go
    Let _ _ x
     -> goX x >> go

 where
  inv = checkInvariants ctx
  q' = Query cs xfinal
  go = invariantQ ctx q'
  goX = invariantX ctx

  goBanAll
     = flip invariantQ q'
     $ ctx { checkInvariants = inv { allowWindows = False
                                   , allowGroupFolds = False }}

  goBanWindow
     = flip invariantQ q'
     $ ctx { checkInvariants = inv { allowWindows = False }}

  goBanWindowAndGroupFold
     = flip invariantQ q'
     $ ctx { checkInvariants = inv { allowWindows = False
                                   , allowGroupFolds = False }}

  errBanWindow sug
   = errorSuggestions
      (ErrorContextNotAllowedHere (annotOfContext c) c)
      [ Suggest "Windows cannot be inside groups/latest."
      , Suggest sug]

  errBanGroupFold
   = errorSuggestions
      (ErrorContextNotAllowedHere (annotOfContext c) c)
      [ Suggest "Group folds are unsupported inside groups/latests." ]


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
