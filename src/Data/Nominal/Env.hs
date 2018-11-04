module Data.Nominal.Env
  where

import qualified Data.Nominal.Perm as P
import qualified Data.Set          as S
import qualified Data.Nominal.Set  as NS

data Env a = Env { perm     :: P.Perm a
                 , freshSet :: S.Set a
                 }
  deriving (Eq,Ord,Show,Read)

empty :: (Ord a) => Env a
empty = Env P.identity S.empty

inverse :: (Ord a) => Env a -> Env a
inverse (Env p s) = Env (P.inverse p) s

image :: (Ord a) => Env a -> a -> a
image = P.image . perm

imageInv :: (Ord a) => Env a -> a -> a
imageInv = P.imageInv . perm

swapList :: (Ord a) => Env a -> [(a,a)]
swapList = P.toList . perm

swapL :: (Ord a) => a -> a -> Env a -> Env a
swapL a b (Env p s) = Env (P.swapL a b p) s

swapR :: (Ord a) => a -> a -> Env a -> Env a
swapR a b (Env p s) = Env (P.swapR a b p) (NS.swap a b s)

isFresh :: (Ord a) => a -> Env a -> Bool
isFresh a = (S.member a) . freshSet

setFresh :: (Ord a) => Bool -> a -> Env a -> Env a
setFresh b a (Env p s) = Env p (NS.setMember b a s)

