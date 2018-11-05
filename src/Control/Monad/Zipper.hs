module Control.Monad.Zipper where

import           Control.Monad.ContState
import           Control.Monad.State
import qualified Data.Zipper                   as Z

zip'path :: CS r (ExtB l e (StateT (Z.Zipper term dir m1))) m [dir]
zip'path = getsL Z.zip'path

zip'term :: CS r (ExtB l e (StateT (Z.Zipper term dir m1))) m term
zip'term = getsL Z.zip'term

zip'cont
  :: CS
       r
       (ExtB l e (StateT (Z.Zipper term dir m1)))
       m
       (Z.ZipReq term dir -> m1 (Z.Zipper term dir m1))
zip'cont = getsL Z.zip'cont

zip'update
  :: term -> CS r (ExtB l e (StateT (Z.Zipper term dir (CS r l m)))) m ()
zip'update t = mmodifyL $ Z.zip'update t

zip'isRoot
  :: (Monad m1) => CS r (ExtB l e (StateT (Z.Zipper term dir m1))) m Bool
zip'isRoot = getsL Z.zip'isRoot

zip'move
  :: Z.Dir dir -> CS r (ExtB l e (StateT (Z.Zipper term dir (CS r l m)))) m ()
zip'move d = mmodifyL $ Z.zip'move d

zip'moves
  :: [Z.Dir dir] -> CS r (ExtB l e (StateT (Z.Zipper term dir (CS r l m)))) m ()
zip'moves ld = mmodifyL $ Z.zip'moves ld

zip'toRoot :: CS r (ExtB l e (StateT (Z.Zipper term dir (CS r l m)))) m ()
zip'toRoot = mmodifyL Z.zip'toRoot

runZipL
  :: (Z.ZipperAble term dir)
  => term
  -> CS r1 (ExtB l1 a (StateT (Z.Zipper term dir (CS r l m)))) m a
  -> CS r1 l1 m a
runZipL t m = fst <$> runStateL (Z.zipper t) m
