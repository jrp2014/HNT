{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,
  FlexibleContexts #-}module Nominal.Rewriting where

import Hnt.Utils.Parser
import Text.ParserCombinators.Parsec

import           Data.Nominal.Term
import qualified Data.Nominal.FrsCtxt as FC

import Control.Monad.Trans.Except
import Control.Monad.State
import Control.Monad.ContState

import Control.Monad.Nominal.Sol

import Nominal.Matching
import Data.Maybe


-- * Rewriting Data Structures

-- | A nominal rewriting rule
data Rule atm cst var   = Rule  { rule'ctxt  :: FC.FrsCtxt var atm, -- ^ context of the rule
                                  -- | left-hand side term, the one that is matched
                                  rule'left  :: Term atm cst var  ,
                                  -- | right-hand side term
                                  rule'right :: Term atm cst var
                                }
  deriving (Ord,Eq)

-- | A term with a Freshness Context
data CtxtTerm atm cst var = CtxtTerm { -- | the context
                                       ct'ctxt :: FC.FrsCtxt var atm,
                                       -- | the term
                                       ct'term :: Term atm cst var
                                     }
 deriving (Eq,Ord)


-- * Parsing Functions


instance (Show atm, Show cst, Show var) => Show (Rule atm cst var) where
  show (Rule fc l r) = show fc ++ " |- " ++ show l ++ " -> " ++ show r


instance (Show atm, Show cst, Show var) => Show (CtxtTerm atm cst var) where
  show (CtxtTerm ctxt term) = show ctxt ++ " |- " ++ show term


parse'rule :: (Ord var, Ord atm, ParsecRead atm, ParsecRead cst, ParsecRead var) =>
                 GenParser Char st (Rule atm cst var)
parse'rule =  (do mc <- optionMaybe (do c <- FC.parse'frsCtxt
                                        spaces >> string "|-" >> spaces
                                        return c
                                    )
                  let c = fromMaybe FC.empty mc
                  t <- parse'term
                  spaces >> string "->" >> spaces
                  Rule c t <$> parse'term
              ) <?> "Rule"


parse'ctxtTerm :: (Ord var, Ord atm, ParsecRead atm, ParsecRead cst, ParsecRead var) =>
                 GenParser Char st (CtxtTerm atm cst var)
parse'ctxtTerm =  (do mc <- optionMaybe (do c <- FC.parse'frsCtxt
                                            spaces >> string "|-" >> spaces
                                            return c
                                        )
                      let c = fromMaybe FC.empty mc
                      CtxtTerm c <$> parse'term
                  ) <?> "Term in Context"




instance (Ord atm, Ord var, ParsecRead atm, ParsecRead cst, ParsecRead var) => ParsecRead (Rule atm cst var) where
  parsecRead = parse'rule

instance (Ord atm, Ord var, ParsecRead atm, ParsecRead cst, ParsecRead var) => ParsecRead (CtxtTerm atm cst var) where
  parsecRead = parse'ctxtTerm


-- * Rewriting Functions

rewrite :: (Ord t1, Ord t, Show t2, Eq t2) => Rule t1 t2 t -> Term t1 t2 t
           -> CS r (ExtB (ExtB l e1 (ExceptT String)) e (StateT (FC.FrsCtxt t t1))) m (Term t1 t2 t)
rewrite (Rule fc l r) t = do frs <- getL
                             s   <- inc $ match'check (fc , l) (frs , t)
                             substituteM (return . substValuef s) r

rewrite'empty :: (Ord t1, Ord t, Show t2, Eq t2) =>
                 Rule t1 t2 t
                 -> Term t1 t2 t
                 -> CS r (ExtB l e1 (ExceptT String)) m (Term t1 t2 t)
rewrite'empty rl t = runFrsCtxtL $ rewrite rl t

