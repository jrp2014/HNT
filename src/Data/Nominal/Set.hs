module Data.Nominal.Set where

import qualified Data.Set as Set

import Data.Maybe

setMember :: (Ord a) => Bool -> a -> Set.Set a -> Set.Set a
setMember True  = Set.insert
setMember False = Set.delete

swap :: (Ord a) => a -> a -> Set.Set a -> Set.Set a
swap a b s = setMember ma b (setMember mb a s)
  where ma = Set.member a s
	mb = Set.member b s

pick :: (Ord a) => Set.Set a -> Maybe a
pick = listToMaybe . Set.elems
