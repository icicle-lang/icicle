{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Core.Program.Simp
     ( simpProgram
     ) where


import           Icicle.Common.Fresh
import           Icicle.Core.Program.Program
import           Icicle.Core.Stream.Stream
import qualified Icicle.Core.Exp.Simp as S
import qualified Icicle.Core.Exp.Exp as C

import           P

import           Data.Hashable (Hashable)


simp :: (Hashable n, Eq n) => a -> C.Exp a n -> Fresh n (C.Exp a n)
simp = S.simp

-- | Simplifies individual exps in the Core prorgam.
--
--   note: I don't know how this should handle bindings
--         especially across across stages...
--         say precomps have: x = 1, y = 2, z = x + y
--         we probably don't want to get rid of the names x and y
--         but maybe we do want to simplify z?
--         --tranma
--
simpProgram :: (Hashable n, Eq n) => a -> Program a n -> Fresh n (Program a n)
simpProgram a_fresh p
  = do pres <- forAll simp       (precomps  p)
       poss <- forAll simp       (postcomps p)
       ss   <- mapM (simpStream a_fresh) (streams   p)
       rets <- forAll simp       (returns   p)
       return p { precomps  = pres
                , streams   = ss
                , postcomps = poss
                , returns   = rets }
  where forAll f = traverse (traverse (f a_fresh))


-- | Simp the exps in stream
--
simpStream :: (Hashable n, Eq n) => a -> Stream a n -> Fresh n (Stream a n)
simpStream a_fresh ss
 = case ss of
  SFold n t z k
   -> SFold n t <$> simp a_fresh z <*> simp a_fresh k
  SFilter x ss'
   -> SFilter <$> simp a_fresh x <*> mapM (simpStream a_fresh) ss'

