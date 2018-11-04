{-# LANGUAGE FlexibleInstances #-}
module Hnt.Utils.Parser where

import Text.ParserCombinators.Parsec


class ParsecRead t where
  parsecRead :: GenParser Char st t


instance ParsecRead Integer where
  parsecRead = do s <- option False (char '-' >> spaces >> return True)
                  n <- many1 digit >>= return . read
                  if s
                   then return $ - n
                   else return $   n


instance (ParsecRead t) => ParsecRead [t] where
 parsecRead = parse'list parsecRead


word :: GenParser Char st [Char]
word   = many1 alphaNum <?> "identifier"


parse'seq :: GenParser Char st t -> GenParser Char st [t]
parse'seq p = ( option [] ( do x <- p
                               spaces
                               l <- option [] ((parse'seq p) <?> "End of Seq")
                               return (x : l)
                          )
              ) <?> "Sequence" 


-- parse'seq :: GenParser Char st t -> GenParser Char st [t]
-- parse'seq p = ( option [] ( do x <- p
--                                spaces
--                                l <- option [] ((char ',' >> spaces >> parse'seq p)
--                                                <?> "End of Seq")
--                                return (x : l)
--                           )
--               ) <?> "Sequence" 


parse'list :: GenParser Char st t -> GenParser Char st [t]
parse'list p = ( do char '[' >> spaces
                    t <- parse'seq p
                    spaces   >> char ']'
                    return t
               ) <?> "List"

parse'set :: GenParser Char st t -> GenParser Char st [t]
parse'set p = ( do char '{' >> spaces
                   t <- parse'seq p
                   spaces   >> char '}'
                   return t
              ) <?> "Set"


parseWithSpace :: GenParser Char () a -> [Char] -> Either ParseError a
parseWithSpace parser = parse (do spaces
                                  t <- parser
                                  spaces
                                  eof
                                  return t
                               ) []


parse'read :: GenParser Char () t -> String -> [(t, String)]
parse'read parser = readParen False $ (either (const $ []) id) . (parse parser' "")
 where
  parser' = do spaces
               l <- parser
               r <- getInput
               return [(l,r)]


readViaParsec :: (ParsecRead t) => String -> t
readViaParsec s = either (const $ error "Read failed") id
                         (parse (do spaces
                                    r <- parsecRead
                                    spaces >> eof
                                    return r
                                 ) "" s)