{-# LANGUAGE RankNTypes, MultiParamTypeClasses, FlexibleContexts, KindSignatures #-}
-- | This module is inspired by the slides
-- | /Monadic Reflection in Haskell/ by Andrzej Filinski
-- | Available at <http://cs.ioc.ee/mpc-amast06/msfp/filinski-slides.pdf>
module Control.Monad.ContState.CS where

import Control.Monad()
import Control.Monad.Trans
import Control.Monad.Identity

newtype CS r l m a = CS { rCS :: (a -> l m r -> m r) -> l m r -> m r }

instance Functor (CS r l m) where
  fmap = liftM

instance Applicative (CS r l m) where
 pure x     = CS $ ($ x)
 (<*>) = ap 

instance Monad (CS r l m) where
 (CS m) >>= f = CS $ \k -> m (\a -> rCS (f a) k)

instance MonadTrans (CS r l) where
 lift m = CS $ \k s -> m >>= (\a -> k a s)

type UP r = ()

data Pure m r = Pure (UP (m r))
 deriving (Eq,Ord,Show,Read)
newtype ExtB l e t m r = ExtB { rExtB :: (t (CS r l m) e -> l m r -> m r, l m r) }

class (Monad b, Monad (t b), MonadTrans t) => MonadL t b where
  glue ::   b (t  b a) -> t b a -- struct. map of b-algebra t b a
  glue = join . lift
  -- ax1:   glue . return = id
  -- ax2:   glue  (b >>= f) = glue (b >>= (return . glue . f))
  -- ax3:   glue  b >>= f = glue (b >>= return . (>>= f))

-- instance MonadT t b => MonadL t b where
--   glue = join . lift'
-- instance MonadL t b => MonadT t b where
--   lift' b = glue (b >>= return . return)


lreflect :: MonadL t (CS r l m) => t (CS r l m) a -> CS r (ExtB l e t) m a
lreflect t = CS $ \k (ExtB (mk,s)) -> mk (t >>= \a -> glue (CS $ \k' s' -> k a (ExtB (k',s'))))  s

lreify :: MonadL t (CS r l m) => CS r (ExtB l a t) m a -> t (CS r l m) a
lreify t = glue (CS $ \k s -> rCS t (\a (ExtB (mk,s')) ->  mk (return a) s' ) (ExtB (k,s)) )

inc :: CS r l m a -> CS r (ExtB l e t) m a
inc m = CS $ \k (ExtB (n,s)) -> rCS m (\x s' ->  k x (ExtB (n,s'))) s

runCSM :: (Monad m) => (forall r . CS r Pure m a) -> m a
runCSM m = rCS m (\a (Pure ()) -> return a) (Pure ())

runCS :: (forall m r . CS r Pure m t) -> t
runCS m = runIdentity $ runCSM m

superL ::    (forall a b . CS r l m a -> CS r l m b)
          -> (forall a b . CS r (ExtB l e t) m a -> CS r (ExtB l e t) m b)
superL f y = join $ inc $ f $ return y


--
-- Specified
--

returnCS :: a -> CS r l m a
returnCS = return

joinCS :: CS r l m (CS r l m a) -> CS r l m a
joinCS   = join

liftCS :: (Monad m) => m a -> CS r l m a
liftCS = lift

join'inc :: CS r l m (CS r (ExtB l t t1) m a) -> CS r (ExtB l t t1) m a
join'inc m = CS $ \k'' (ExtB (n,s)) -> rCS m (\x s' ->  rCS x k'' (ExtB (n,s'))) s
