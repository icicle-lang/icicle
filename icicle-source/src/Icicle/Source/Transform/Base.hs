{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Source.Transform.Base (
    Transform(..)
  , idTransform
  , transformQT
  , transformQ
  , transformF
  , transformC
  , transformX
  ) where

import Icicle.Source.Query
import Icicle.Source.Type.Base (Annot)

import P

data Transform m c a n
 = Transform
 { transformExp     :: c
                    -> Exp a n
                    -> m (c, Exp a n)
 , transformPat     :: c
                    -> Pattern n
                    -> m (c, Pattern n)
 , transformContext :: c
                    -> Context a n
                    -> m (c, Context a n)
 , transformState   :: c
 }

idTransform :: Monad m => Transform m () a n
idTransform
 = Transform ret ret ret ()
 where
  ret s x = return (s,x)


transformQT
    :: (Monad m)
    => Transform m c a n
    -> QueryTop a n
    -> m (QueryTop a n)
transformQT t (QueryTop f qn q)
 = do   q' <- transformQ t q
        return $ QueryTop f qn q'

transformQ
    :: (Monad m)
    => Transform m c a n
    -> Query a n
    -> m (Query a n)
transformQ t (Query ctxs xx)
 = do   (s',cs')  <- goCS (transformState t) ctxs
        xx' <- transformX (t { transformState = s' }) xx
        return $ Query cs' xx'
 where
  goCS s []
   = return (s, [])
  goCS s (c:cs)
   = do (s',c') <- transformC (t { transformState = s }) c
        (s'',cs') <- goCS s' cs
        return (s'', c' : cs')


transformF
    :: (Monad m)
    => Transform m c (Annot a n) n
    -> ResolvedFunction a n
    -> m (ResolvedFunction a n)
transformF t (ResolvedFunction a n typ q)
 = do   q' <- transformX t q
        return $ ResolvedFunction a n typ q'


transformC
    :: (Monad m)
    => Transform m c a n
    -> Context a n
    -> m (c, Context a n)
transformC t cc
 = do (s,cc') <- transformContext t (transformState t) cc
      let goX = transformX (t { transformState = s })
      case cc' of
       Windowed{} -> return (s,cc')
       Latest{} -> return (s,cc')
       GroupBy a x
        -> do x' <- goX x
              return (s, GroupBy a x')
       GroupFold a k v x
        -- Note: expression x does not see the outer state
        --       context in its transformation as it happens
        --       `before` the group fold operation.
        --       This is important for the inliner, as
        --       > group fold (k, sum) = sum severity in sum
        --       should have the original sum inside.
        -> do x' <- transformX t x
              return (s, GroupFold a k v x')
       Distinct a x
        -> do x' <- goX x
              return (s, Distinct a x')
       Filter a x
        -> do x' <- goX x
              return (s, Filter a x')
       LetFold a f
        -> do  i' <- goX $ foldInit f
               w' <- goX $ foldWork f
               return (s, LetFold a (f { foldInit = i', foldWork = w' }))
       Let a n x
        -- Note: see above. Let bindings are non-recursive, so
        --       we need to inline only subsequent declarations,
        --       not the internal one.
        -> do  x' <- transformX t x
               return (s, Let a n x')


transformX
    :: (Monad m)
    => Transform m c a n
    -> Exp a n
    -> m (Exp a n)
transformX t xx
 = do (s',xx') <- transformExp t (transformState t) xx
      let t' = t { transformState = s' }
      let goQ = transformQ t'
      let goX = transformX t'
      let goP (p,alt)
              = do  (sp,p') <- transformPat t s' p
                    alt'    <- transformX (t { transformState = sp }) alt
                    return (p', alt')

      case xx' of
       Var{} -> return xx'
       Lam a n p
        -> do p' <- goX p
              return $ Lam a n p'
       Nested a q
        -> do q' <- goQ q
              return $ Nested a q'
       App a p q
        -> do p' <- goX p
              q' <- goX q
              return $ App a p' q'
       Prim{}
        -> return xx'
       If a scr true false
        -> do scr'   <- goX scr
              true'  <- goX true
              false' <- goX false
              return $ If a scr' true' false'
       Case a scr pats
        -> do scr'  <- goX scr
              pats' <- mapM goP pats
              return $ Case a scr' pats'
       Access a x f
        -> do x' <- goX x
              return $ Access a x' f
