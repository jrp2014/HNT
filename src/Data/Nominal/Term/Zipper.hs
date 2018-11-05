{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Nominal.Term.Zipper where

import Data.Nominal.Term.Term
import Data.Zipper

import Text.ParserCombinators.Parsec
import Hnt.Utils.Parser


data TermDir = PairLeft | PairRight | ModalDown
  deriving (Eq,Ord,Read)


instance Show TermDir where
 show PairLeft  = "left"
 show PairRight = "right"
 show ModalDown = "modal"

parse'termDir :: GenParser Char st TermDir
parse'termDir = (    (string "left"  >> return PairLeft  )
                 <|> (string "right" >> return PairRight )
                 <|> (string "modal" >> return ModalDown )
                ) <?> "left, right or modal"


instance ParsecRead TermDir where
 parsecRead = parse'termDir


instance ZipperAble (Term atm cst var) TermDir where
  possibleDirs (Leaf    _) = []
  possibleDirs (Modal m s) = [(ModalDown , (s , Modal m)) ]
  possibleDirs (Pair  s u) = [(PairLeft  , (s , (`Pair` u))) ,
                              (PairRight , (u , Pair  s))
                             ]
