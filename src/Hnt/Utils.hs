module Hnt.Utils
  ( module Hnt.Utils.Parser
  , printSeq
  , ignoreR
  , localM
  , lprint
  , while
  , whileM
  , doWhileM
  , loopM
  , ifM
  , eitherUntilM
  , repeatM
  , whenM
  )
 where

import Hnt.Utils.Parser


import Control.Monad
import Control.Monad.Trans

printSeq :: [a] -> [a] -> [a] -> [[a]] -> [a]
printSeq open close sep sl = open ++ printSeq' sl
  where printSeq'  []   = close
        printSeq'  [x]  = x ++ close
        printSeq' (x:l) = x ++ sep ++ printSeq' l


ignoreR :: (Monad t) => t t2 -> t t1 -> t t2
ignoreR m n = do r <- m ; n ; return r

localM :: (Monad t) => t t1 -> (t1 -> t t2) -> t t3 -> t t3
localM fdo fundo k = do sv <- fdo ; ignoreR k (fundo sv)



-- * Monads

lprint :: (Show a, MonadIO m) => a -> m ()
lprint = liftIO . print

while :: (a -> Bool) -> (a -> a) -> a -> a
while predicate f = while'
  where
    while' a = if predicate a then while' (f a) else a

-- * STATE GEN

whileM :: (Monad m) => (a -> m Bool) -> (a -> m a) -> a -> m a
whileM predicate f = whileM'
  where
    whileM' a = do b <- predicate a
                   if b then f a >>= whileM' else return a

doWhileM :: (Monad m) => (a -> m Bool) -> (a -> m a) -> a -> m a
doWhileM predicate f a = f a >>= whileM predicate f

loopM :: (Monad m) => (a -> m a) -> a -> m b
loopM f = loopM'
  where loopM' a = f a >>= loopM'

repeatM :: (Monad m) => m a -> m b
repeatM m = m >> repeatM m

ifM :: (Monad m) => (a -> m Bool) -> (a -> m b) -> (a -> m b) -> (a -> m b)
ifM predicate thenM elseM a = do b <- predicate a
                                 if b then thenM a
                                      else elseM a


whenM :: (Monad m) => m Bool -> m () -> m ()
whenM mb mu = mb >>= (`when` mu)

eitherUntilM :: (Monad m) => (a -> m c) -> m (Either a b) -> m b
eitherUntilM err f = aux
 where
   aux = f >>= either (\e -> err e >> aux) return
