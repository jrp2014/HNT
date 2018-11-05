module Control.Monad.Nominal.Env where

import Data.Nominal.Perm(Perm)
import qualified Data.Nominal.Env as E
import Control.Monad.ContState
import Control.Monad.State

import qualified Data.Set as Set


image :: (Ord a) => a -> CS r (ExtB l e (StateT (E.Env a))) m a
image    a = getsL $ flip E.image a

perm :: CS r (ExtB l e (StateT (E.Env a))) m (Perm a)
perm       = getsL E.perm


swapList :: (Ord a) => CS r (ExtB l e (StateT (E.Env a))) m [(a, a)]
swapList   = getsL E.swapList

swapR :: (Ord a) => a -> a -> CS r (ExtB l e (StateT (E.Env a))) m ()
swapR a b = modifyL $ E.swapR a b

swapL :: (Ord a) => a -> a -> CS r (ExtB l e (StateT (E.Env a))) m ()
swapL a b = modifyL $ E.swapL a b


localSwapR :: (Ord a) => a -> a -> CS r (ExtB l e (StateT (E.Env a))) m t
                                -> CS r (ExtB l e (StateT (E.Env a))) m t
localSwapR a b m = do swapR a b
                      r <- m
                      swapR a b
                      return r

localSwapL :: (Ord a) => a -> a -> CS r (ExtB l e (StateT (E.Env a))) m t
                                -> CS r (ExtB l e (StateT (E.Env a))) m t
localSwapL a b m = do swapL a b
                      r <- m
                      swapL a b
                      return r



isFresh :: (Ord a) => a -> CS r (ExtB l e (StateT (E.Env a))) m Bool
isFresh  a = getsL (E.isFresh a)

freshSet :: CS r (ExtB l e (StateT (E.Env a))) m (Set.Set a)
freshSet = getsL E.freshSet


setFresh :: (Ord a) => Bool -> a -> CS r (ExtB l e (StateT (E.Env a))) m ()
setFresh b a  = modifyL $ E.setFresh b a

localSetFresh :: (Ord a) =>
                 Bool
                 -> a
                 -> CS r (ExtB l e (StateT (E.Env a))) m t
                 -> CS r (ExtB l e (StateT (E.Env a))) m t
localSetFresh b a m = do b' <- isFresh a
                         setFresh b  a
                         r <- m
                         setFresh b' a
                         return r

localSetFreshS :: (Ord a) =>   Set.Set (Bool, a)
                            -> CS r (ExtB l e (StateT (E.Env a))) m t
                            -> CS r (ExtB l e (StateT (E.Env a))) m t
localSetFreshS = flip $ Set.fold (uncurry localSetFresh)


runEnvL :: (Ord a) => CS r (ExtB l a1 (StateT (E.Env a))) m a1 -> CS r l m a1
runEnvL m = fst <$> runStateL E.empty m


