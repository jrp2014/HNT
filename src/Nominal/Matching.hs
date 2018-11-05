{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
module Nominal.Matching where

import Data.Nominal.Term

import qualified Data.Nominal.Env     as E
import qualified Data.Nominal.Perm    as P
import qualified Data.Nominal.FrsCtxt as FC

import qualified Data.Set as Set

import Control.Monad.Trans.Except
import Control.Monad.State
import Control.Monad.ContState

import Control.Monad.Nominal.Env
import Control.Monad.Nominal.Sol


-- LAYERS -> INNER : SOL
--                 : ERROR
--                 : CONTEXT
--                 : ENV

-- * Core Functions


checkAtom ::               
  (Ord a) =>               
  a                        
  -> CS                    
       r                   
       (ExtB               
          (ExtB (ExtB l e11 (ExceptT [Char])) e1 t1) e (StateT (E.Env a)))
       m                                                                 
       a                                                                 
checkAtom a = do frs <- isFresh a
                 if frs
                  then inc2 $ throwErrorL "Freshness error"
                  else image a






checkVar ::                                                              
  (Ord a) =>                                                             
  (var -> CS r (ExtB l e (StateT (E.Env a))) m t)                        
  -> var                                                                 
  -> CS r (ExtB l e (StateT (E.Env a))) m (Term a cst var)               
checkVar frsConst v = do frsConst v
                         sl <- swapList
                         return $ foldr (\(a,b) t -> swap a b t) (var v) sl





checkTerm ::                                                             
  (Ord a) =>                                                             
  (var                                                                   
   -> CS                                                                 
        r                                                                
        (ExtB                                                            
           (ExtB (ExtB l e (ExceptT [Char])) e1 t1) e11 (StateT (E.Env a)))
        m                                                                 
        t)                                                                
  -> Term a cst var                                                       
  -> CS                                                                   
       r                                                                  
       (ExtB                                                              
          (ExtB (ExtB l e (ExceptT [Char])) e1 t1) e11 (StateT (E.Env a))) 
       m                                                                  
       (Term a cst var)                                                   
checkTerm frsConst = checkTerm'
  where
    checkTerm' (Modal (Swap a b) t) = localSwapR a b (checkTerm' t)
    checkTerm' (Modal (Abs    a) t) = do b <- image a
                                         u <- localSetFresh False a (checkTerm' t)
                                         return (Modal (Abs b) u)
    checkTerm' (Pair s t)           = do s' <- checkTerm' s
                                         t' <- checkTerm' t
                                         return (Pair s' t')
    checkTerm' (Leaf (Atm a))       = checkAtom a >>= return . atm
    checkTerm' (Leaf (Var v))       = checkVar frsConst v 
    checkTerm'  t                   = return t

core ::                                                                   
  (Eq t1, Show t, Eq t, Ord a) =>                                         
  (t1                                                                     
   -> Term a t t1                                                         
   -> CS                                                                  
        r                                                                 
        (ExtB                                                             
           (ExtB (ExtB l e11 (ExceptT [Char])) e1 t11) e (StateT (E.Env a)))
        m                                                                  
        ())                                                                
  -> (t1                                                                   
      -> CS                                                                
           r                                                               
           (ExtB                                                           
              (ExtB (ExtB l e11 (ExceptT [Char])) e1 t11) e (StateT (E.Env a)))
           m                                                                  
           ())                                                                
  -> Term a t t1                                                              
  -> Term a t t1                                                              
  -> CS                                                                       
       r                                                                      
       (ExtB                                                                  
          (ExtB (ExtB l e11 (ExceptT [Char])) e1 t11) e (StateT (E.Env a)))    
       m                                                                      
       ()                                                                     
core fsubst frsConst = core'
  where
    core' (Modal (Swap a b) t)                   u   = localSwapL a b (core' t u)
    core'                   t  (Modal (Swap a b) u)  = localSwapR a b (core' t u)
    core' (Modal (Abs    a) t) (Modal (Abs    b) u)  = localSetFresh True  a (
                                                       localSetFresh False b (
                                                           localSwapL a b  (core' t u)
                                                        ) )
    core' (Pair           s t) (Pair           u v)  = core' s u >> core' t v
    core' (Leaf (Cst       f)) (Leaf (Cst       g))  = when (f /= g)
                                                        (inc2 $ throwErrorL   (   "Error : "
                                                                               ++ (show f)
                                                                               ++ " /= "
                                                                               ++ (show g)
                                                                              )
                                                        )
    core' (Leaf (Atm       a)) (Leaf (Atm       b))  = do c <- checkAtom b
                                                          when (a /= c)
                                                           (inc2 $ throwErrorL "Atoms not equals")
    core' t@(Leaf (Var     v))                  u    = do  if t == u
                                                            then (do p <- perm
                                                                     localSetFreshS
                                                                      (Set.map (\a -> (True,a)) $ P.supp p)
                                                                      (frsConst v)
                                                                 )
                                                            else fsubst v u
    core'                  _    _                    = inc2 $ throwErrorL "Incompatible constructors"
 


core'run ::                                                                   
  (Eq t1, Show t1, Eq t, Ord a) =>                                            
  (t                                                                          
   -> Term a t1 t                                                             
   -> CS                                                                      
        r                                                                     
        (ExtB                                                                 
           (ExtB (ExtB l e (ExceptT [Char])) e1 t11) () (StateT (E.Env a)))    
        m                                                                     
        ())                                                                   
  -> (t                                                                       
      -> CS                                                                   
           r                                                                  
           (ExtB                                                              
              (ExtB (ExtB l e (ExceptT [Char])) e1 t11) () (StateT (E.Env a))) 
           m                                                                  
           ())                                                                
  -> Term a t1 t                                                              
  -> Term a t1 t                                                              
  -> CS r (ExtB (ExtB l e (ExceptT [Char])) e1 t11) m ()                       
core'run f g t u = runEnvL $ core f g t u


-- * Alpha and Matching functions


checkSatisfied ::                                                             
  (Ord v, Ord a) =>                                                           
  v                                                                           
  -> CS                                                                       
       r                                                                      
       (ExtB                                                                  
          (ExtB (ExtB l e11 (ExceptT [Char])) e1 (StateT (FC.FrsCtxt v a)))    
          e                                                                   
          (StateT (E.Env a)))                                                 
       m                                                                      
       ()                                                                     
checkSatisfied v = do b <- freshSet >>= inc . isInFrsCtxt v
                      when (not b) (inc2 $ throwErrorL "Constraints not satisfied")


fsubst'error ::                                                               
  t                                                                           
  -> t1                                                                       
  -> CS r (ExtB (ExtB (ExtB l e11 (ExceptT [Char])) e1 t11) e t2) m a          
fsubst'error _ _ = inc2 $ throwErrorL "Trying to Subst !"


addFrsCtxtEnv ::                                                              
  (Ord v, Ord a) =>                                                           
  v                                                                           
  -> CS                                                                       
       r                                                                      
       (ExtB (ExtB l e11 (StateT (FC.FrsCtxt v a))) e (StateT (E.Env a)))     
       m                                                                      
       ()                                                                     
addFrsCtxtEnv v = freshSet >>= inc . (addFrsCtxt v)

alpha'solve ::                                                                
  (Show t1, Eq t1, Ord a, Ord t) =>                                           
  Term a t1 t                                                                 
  -> Term a t1 t                                                              
  -> CS r (ExtB l e (ExceptT [Char])) m (FC.FrsCtxt t a)                       
alpha'solve t u = runFrsCtxtL $ do core'run fsubst'error addFrsCtxtEnv t u
                                   getL

alpha'check ::                                                                
  (Show t, Eq t, Ord a, Ord v) =>                                             
  FC.FrsCtxt v a                                                              
  -> Term a t v                                                               
  -> Term a t v                                                               
  -> CS r (ExtB l e (ExceptT [Char])) m ()                                     
alpha'check frsctxt t u = runFrsCtxtL $ (putL frsctxt >> core'run fsubst'error checkSatisfied t u)


fsubst'solve ::                                                               
  (Eq cst, Show cst, Ord v, Ord var, Ord a) =>                                
  FC.FrsCtxt v a                                                              
  -> v                                                                        
  -> Term a cst var                                                           
  -> CS                                                                       
       r                                                                      
       (ExtB                                                                  
          (ExtB                                                               
             (ExtB                                                            
                (ExtB l e (StateT (Subst v (Term a cst var)))) e1 (ExceptT [Char]))
             e11                                                                  
             (StateT (FC.FrsCtxt var a)))                                         
          e2                                                                      
          (StateT (E.Env a)))                                                     
       m                                                                          
       ()                                                                         
fsubst'solve ctxtp v t =
   do u <- inc3 $ substValue v
      case u of
       Nothing -> do let fcset = FC.frsVarSet v ctxtp
                     p  <- perm
                     let set   = Set.map (P.image (P.inverse p)) fcset
                     t' <- localSetFreshS (Set.map (\a -> (True , a)) set)
                                          (checkTerm addFrsCtxtEnv t)
                     inc3 $ addSubst v t'
       Just u' -> do t' <- checkTerm addFrsCtxtEnv t
                     -- alpha'solve
                     localL (putL E.empty >> core fsubst'error addFrsCtxtEnv u' t')



match'solve ::                                                                    
  (Ord a, Ord v, Show t, Eq t) =>                                                 
  FC.FrsCtxt v a                                                                  
  -> Term a t v                                                                   
  -> Term a t v                                                                   
  -> CS                                                                           
       r                                                                          
       (ExtB l e (ExceptT [Char]))                                                 
       m                                                                          
       (FC.FrsCtxt v a, Subst v (Term a t v))                                     
match'solve ctxtp t u = shiftErrorL $ runSubstL $ runErrorL $ runFrsCtxtL (
  do core'run (fsubst'solve ctxtp) addFrsCtxtEnv t u
     frs   <- getL
     subst <- inc2 $ getL
     return (frs,subst)
 )

 
fsubst'check ::                                                                   
  (Eq cst, Show cst, Ord v, Ord var, Ord a) =>                                    
  FC.FrsCtxt v a                                                                  
  -> v                                                                            
  -> Term a cst var                                                               
  -> CS                                                                           
       r                                                                          
       (ExtB                                                                      
          (ExtB                                                                   
             (ExtB
                (ExtB l e (StateT (Subst v (Term a cst var)))) e1 (ExceptT [Char]))
             e11
             (StateT (FC.FrsCtxt var a)))
          e2
          (StateT (E.Env a)))
       m
       ()
fsubst'check ctxtp v t =
  do u <- inc3 $ substValue v
     case u of
      Nothing -> do let fcset = FC.frsVarSet v ctxtp
                    p  <- perm
                    let set   = Set.map (P.image (P.inverse p)) fcset
                    t' <- localSetFreshS (Set.map (\a -> (True , a)) set)
                                         (checkTerm checkSatisfied t)
                    inc3 $ addSubst v t'
      Just u' -> do t' <- checkTerm checkSatisfied t
                    localL (putL E.empty >> core fsubst'error checkSatisfied u' t')


match'check ::
  (Ord a, Ord v, Show t, Eq t) =>
  (FC.FrsCtxt v a, Term a t v)
  -> (FC.FrsCtxt v a, Term a t v)
  -> CS r (ExtB l e (ExceptT [Char])) m (Subst v (Term a t v))
match'check (ctxtp , t) (frsctxt , u) = shiftErrorL $ runSubstL $ runErrorL $ runFrsCtxtL (
  do putL frsctxt
     core'run (fsubst'solve ctxtp) checkSatisfied t u
     inc2 $ getL
 )

match'check'empty ::
  (Eq t, Show t, Ord v, Ord a) =>
  Term a t v
  -> Term a t v
  -> CS r (ExtB l e (ExceptT [Char])) m (Subst v (Term a t v))
match'check'empty t u = match'check (FC.empty , t) (FC.empty , u)
