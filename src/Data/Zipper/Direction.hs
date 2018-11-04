{-# LANGUAGE	DeriveDataTypeable #-}
module Data.Zipper.Direction
  where

import Text.ParserCombinators.Parsec
import Hnt.Utils.Parser


data Dir dir = Up | Down | DownTo dir | Next
  deriving (Eq,Ord,Show,Read)

parse'dir :: GenParser Char st a -> GenParser Char st (Dir a)
parse'dir p = (    (string "up"       >> return Up)
               <|> (string "next"     >> return Next)
               <|> (string "down"     >> (    (try (do spaces >> string "to" >> spaces
                                                       p >>= return . DownTo
                                                   )
                                              )
                                          <|>  return Down
                                         )
                   )
              ) <?> "up, down, down to, next"
                  

instance (ParsecRead a) => ParsecRead (Dir a) where
  parsecRead = parse'dir parsecRead


instance Functor Dir where
 fmap f (DownTo  d) = DownTo (f d)
 fmap _          Up = Up
 fmap _        Next = Next
 fmap _        Down = Down