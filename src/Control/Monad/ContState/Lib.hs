{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables #-}
-- | This module is inspired by the slides
-- | /Monadic Reflection in Haskell/ by Andrzej Filinski
-- | Available at <http://cs.ioc.ee/mpc-amast06/msfp/filinski-slides.pdf>
module Control.Monad.ContState.Lib where

import Control.Monad.ContState.CS
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Cont
import Control.Monad.List


import Data.Monoid

--
-- OUTPUT
--

newtype OutputT o m a = OutputT { runOutputT :: m (a, o) }

instance (Monad m, Monoid o) => Functor (OutputT o m) where
  fmap = liftM

instance (Monad m, Monoid o) => Applicative (OutputT o m) where
  pure a = OutputT (return (a, mempty))
  (<*>) = ap

instance (Monad m, Monoid o) => Monad (OutputT o m) where
  m >>= f = OutputT (do (a,o ) <- runOutputT m
                        (b,o') <- runOutputT (f a)
                        return (b, mappend o o')
                    )


instance (Monoid o) => MonadTrans (OutputT o) where
  lift m = OutputT $ m >>= \a -> return (a,mempty)

instance (Monad m, Monoid o) => MonadL (OutputT o) m

outs :: (Monoid o) => o -> CS r (ExtB l e (OutputT o)) m ()
outs s = lreflect $ OutputT $ return ((), s)

runOutputL :: (Monoid o) => CS r (ExtB l a (OutputT o)) m a -> CS r l m (a, o)
runOutputL = runOutputT . lreify


--
-- LIST
--

instance (Monad m) => MonadL ListT m

pickL :: [a] -> CS r (ExtB l e ListT) m a
pickL l = lreflect $ ListT $ return l

runListL :: CS r (ExtB l a ListT) m a -> CS r l m [a]
runListL = runListT . lreify


--
-- MAYBE
--

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Monad m) => Functor (MaybeT m) where
  fmap = liftM

instance (Monad m) => Applicative (MaybeT m) where
  pure a = MaybeT (return (Just a))
  (<*>) = ap

instance (Monad m) => Monad (MaybeT m) where
  b >>= f = MaybeT (runMaybeT b >>= \m -> case m of
                                           Nothing -> return Nothing
                                           Just a  -> runMaybeT (f a))

instance MonadTrans MaybeT where
 lift m = MaybeT $ m >>= return . Just

instance Monad m => MonadL MaybeT m

raiseL :: CS r (ExtB l e MaybeT) m a
raiseL = lreflect $ MaybeT  $ return Nothing

runMaybeL :: CS r (ExtB l a MaybeT) m a -> CS r l m (Maybe a)
runMaybeL = runMaybeT . lreify

handleL :: CS r (ExtB l a MaybeT) m a -> CS r l m a -> CS r l m a
handleL t1 t2 = (runMaybeL $ t1) >>= \m -> case m of
                                            Just a  -> return a
                                            Nothing -> t2


--
-- CONT
--

instance (Monad m) => MonadL (ContT s) m

runContL :: CS r (ExtB l r1 (ContT r1)) m r1 -> CS r l m r1
runContL m = runContT (lreify m) return

resetL :: CS r (ExtB l r1 (ContT r1)) m r1 -> CS r (ExtB l e t) m r1
resetL = inc . runContL

shiftL :: ((a -> CS r l m r1) -> CS r l m r1) -> CS r (ExtB l e (ContT r1)) m a
shiftL = lreflect . ContT

callccL :: ((a -> CS r1 (ExtB l e1 (ContT r)) m a1) -> CS r1 (ExtB l a (ContT r)) m a)
           -> CS r1 (ExtB l e (ContT r)) m a                                          
callccL e = lreflect $ ContT $ \k -> runContT (lreify  $ e (\x -> lreflect $ ContT $ \_ -> k x)) k


--
-- STATE
--

instance (Monad m) => MonadL (StateT s) m

runStateL :: s -> CS r (ExtB l a (StateT s)) m a -> CS r l m (a, s)
runStateL s m = runStateT (lreify m) s

getL :: CS r (ExtB l e (StateT s)) m s
getL = lreflect $ get

putL :: s -> CS r (ExtB l e (StateT s)) m ()
putL = lreflect . put

localL :: CS r (ExtB l e (StateT s)) m t -> CS r (ExtB l e (StateT s)) m t
localL m = do s <- getL
              r <- m
              putL s
              return r

getsL :: (s -> b) -> CS r (ExtB l e (StateT s)) m b
getsL f = getL >>= return . f 

shiftStateL :: (s -> CS r l m (a, s)) -> CS r (ExtB l e (StateT s)) m a
shiftStateL = lreflect . StateT

modifyL :: (s -> s) -> CS r (ExtB l e (StateT s)) m ()
modifyL f = shiftStateL (\s -> return ((), f s)) 

mmodifyL :: (a -> CS r l m a) -> CS r (ExtB l e (StateT a)) m ()
mmodifyL f = shiftStateL (\s -> f s >>= \s' -> return ((),s'))

popStateL :: CS r (ExtB l e (StateT [a])) m (Maybe a)
popStateL = shiftStateL (\s -> case s of
                                []     -> return (Nothing, [])
                                (t: q) -> return (Just t , q )
                        )


pushStateL :: a -> CS r (ExtB l e (StateT [a])) m ()
pushStateL     = modifyL . (:)

appendStateL :: a -> CS r (ExtB l e (StateT [a])) m ()
appendStateL x = modifyL $ (++ [x])

--
-- Error
--

instance (Monad m, Error e) => MonadL (ErrorT e) m


runErrorL :: (Error e) => CS r (ExtB l a (ErrorT e)) m a -> CS r l m (Either e a)
runErrorL = runErrorT . lreify


throwErrorL :: (Error err) => err -> CS r (ExtB l e (ErrorT err)) m a
throwErrorL = lreflect . throwError

catchErrorL :: (Error err) => CS r (ExtB l a (ErrorT err)) m a
                           -> (err -> CS r (ExtB l a (ErrorT err)) m a)
                           -> CS r (ExtB l e (ErrorT err)) m a
catchErrorL m h = lreflect $ catchError (lreify m) (lreify . h)

shiftErrorL :: (Error e1) => CS r l m (Either e1 a) -> CS r (ExtB l e (ErrorT e1)) m a
shiftErrorL = lreflect . ErrorT

catchErrorLn :: (Monad m, Error err) =>
                (CS r (ExtB l e (ErrorT err)) m1 b -> m (m a)) -> b -> (err -> b) -> m a
catchErrorLn f x h = join $ f $ catchErrorL (return x) (return . h)

--
-- Identity
--

newtype Id m a = Id { rId :: m a }
 deriving (Eq,Ord,Show,Read)

instance (Monad m) => Functor (Id m) where
  fmap = liftM

instance (Monad m) => Applicative (Id m) where
 pure  = Id . return
 (<*>) = ap

instance (Monad m) => Monad (Id m) where
 m >>= f = Id $ (rId m) >>= (rId . f)

instance MonadTrans Id where
 lift = Id

instance (Monad m) => MonadL Id m

runId :: CS r (ExtB l a Id) m a -> CS r l m a
runId = rId . lreify

--
-- IO
--

runCSIO :: (forall r . CS r Pure IO a) -> IO a
runCSIO = runCSM



--
-- Inc
--

inc0 :: CS r l m a -> CS r l m a
inc0 = id

inc1 :: CS r l m a -> CS r (ExtB l e t) m a
inc1 = inc

inc2 :: CS r l m a -> CS r (ExtB (ExtB l e1 t1) e t) m a
inc2 = inc1 . inc1

inc3 :: CS r l m a -> CS r (ExtB (ExtB (ExtB l e2 t2) e t) e1 t1) m a
inc3 = inc2 . inc1

inc4 :: CS r l m a
        -> CS r (ExtB (ExtB (ExtB (ExtB l e2 t2) e3 t3) e t) e1 t1) m a
inc4 = inc2 . inc2

inc5 :: CS r l m a
        -> CS r (ExtB (ExtB (ExtB (ExtB (ExtB l e3 t3) e4 t4) e t) e1 t1) e2 t2) m a
inc5 = inc3 . inc2

inc6 :: CS r l m a
        -> CS
             r
             (ExtB (ExtB (ExtB (ExtB (ExtB (ExtB l e3 t3) e4 t4) e5 t5) e t) e1 t1) e2 t2)
             m
             a
inc6 = inc3 . inc3

inc7 :: CS r l m a
        -> CS
             r
             (ExtB
                (ExtB (ExtB (ExtB (ExtB (ExtB (ExtB l e4 t4) e5 t5) e6 t6) e t) e1 t1) e2 t2)
                e3
                t3)
             m
             a
inc7 = inc4 . inc3

inc8 :: CS r l m a
        -> CS
             r
             (ExtB
                (ExtB
                   (ExtB (ExtB (ExtB (ExtB (ExtB (ExtB l e4 t4) e5 t5) e6 t6) e7 t7) e t) e1 t1)
                   e2
                   t2)
                e3
                t3)
             m
             a
inc8 = inc4 . inc4

inc9 :: CS r l m a
        -> CS
             r
             (ExtB
                (ExtB
                   (ExtB
                      (ExtB (ExtB (ExtB (ExtB (ExtB (ExtB l e5 t5) e6 t6) e7 t7) e8 t8) e t) e1 t1)
                      e2
                      t2)
                   e3
                   t3)
                e4
                t4)
             m
             a
inc9 = inc5 . inc4
