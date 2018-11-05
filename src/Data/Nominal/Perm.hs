module Data.Nominal.Perm where

import qualified Data.Map as Map
import qualified Data.Set as Set

data Perm atm = Perm {  imageMap   :: Map.Map atm atm ,
                        inverseMap :: Map.Map atm atm ,
                        leftSwaps  :: [(atm,atm)],
                        rightSwaps :: [(atm,atm)],
                        supp       :: Set.Set atm
                    }
  deriving (Eq,Ord,Show,Read)

identity :: (Ord a) => Perm a
identity = Perm Map.empty Map.empty [] [] Set.empty

inverse :: (Ord a) => Perm a -> Perm a
inverse (Perm p i l r s)   = Perm i p r l s

image :: (Ord a) => Perm a -> a -> a
image p a = Map.findWithDefault a a (imageMap p)

imageInv :: (Ord a) => Perm a -> a -> a
imageInv = image . inverse

swapR :: (Ord a) => a -> a -> Perm a -> Perm a
swapR a b p@(Perm n i l r s) = deleteEqual a (deleteEqual b (Perm nn ni l ((a,b): r) ns))
  where ia = image p a
        ib = image p b

        nn = Map.insert a ib (Map.insert b ia n)
        ni = Map.insert ib a (Map.insert ia b i)
        ns = Set.insert a    (Set.insert b    s)

        deleteEqual c q@(Perm m j nl nr v) =
          if c == image q c
           then Perm (Map.delete c m) (Map.delete c j) nl nr (Set.delete c v)
           else q

swapL :: (Ord a) => a -> a -> Perm a -> Perm a
swapL a b p = swapR (imageInv p a) (imageInv p b) p


fromList :: (Ord a) => [(a,a)] -> Perm a
fromList []        = identity
fromList ((a,b):l) = swapL a b (fromList l)

toList :: (Ord a) => Perm a -> [(a,a)]
toList p = leftSwaps p ++ rightSwaps p


showPerm :: (Ord a , Show a) => Perm a -> String
showPerm = concatMap (\(a,b) -> "(" ++ show a ++ " " ++ show b ++ ")") . toList
