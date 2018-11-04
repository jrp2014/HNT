module Control.Monad.Nominal.Sol
 where

import qualified Data.Nominal.FrsCtxt as FC
import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad.ContState
import Control.Monad.State

runFrsCtxtL :: CS r (ExtB l a1 (StateT (FC.FrsCtxt v a))) m a1 -> CS r l m a1
runFrsCtxtL m = (runStateL (FC.empty) m) >>= return . fst


isInFrsCtxt :: (Ord a, Ord v) => v -> Set.Set a
                           -> CS r (ExtB l e11 (StateT (FC.FrsCtxt v a))) m Bool
isInFrsCtxt v s = getsL $ FC.isInFrsCtxt v s


addFrsCtxt :: (Ord a, Ord v) => v -> Set.Set a -> CS r (ExtB l e11 (StateT (FC.FrsCtxt v a))) m ()
addFrsCtxt v s = modifyL $ FC.addFrsCtxt v s


type Subst v t = Map.Map v t

substValue :: (Ord v) => v -> CS r (ExtB l e (StateT (Subst v t))) m (Maybe t)
substValue v = getsL $ Map.lookup v

substValuef :: (Ord v) => Subst v t -> v -> Maybe t
substValuef = flip Map.lookup

addSubst :: (Ord v) => v -> t -> CS r (ExtB l e (StateT (Subst v t))) m ()
addSubst v t = modifyL $ Map.insert v t

runSubstL :: CS r (ExtB l a1 (StateT (Subst v t))) m a1 -> CS r l m a1
runSubstL m = (runStateL (Map.empty) m) >>= return . fst
