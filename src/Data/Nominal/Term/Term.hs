

-- | Nominal Terms
module Data.Nominal.Term.Term
  where

import Text.ParserCombinators.Parsec
import Hnt.Utils.Parser

import Control.Monad.Identity
import Data.Maybe

-- * Terms

data Leaf atm cst var = Atm atm -- ^ an atom
                      | Cst cst -- ^ a constant
                      | Var var -- ^ a variable
  deriving (Eq,Ord)

instance (Show atm, Show cst, Show var) => Show (Leaf atm cst var) where
  show (Atm a) = "a:" ++ show a
  show (Var v) = "v:" ++ show v
  show (Cst c) = "c:" ++ show c


data Modal atm = Swap atm atm -- ^ a swapping
               | Abs atm      -- ^ an abstraction
  deriving (Eq,Ord)

instance (Show atm) => Show (Modal atm) where
  show (Abs    a) = "[a:" ++ show a ++ "]"
  show (Swap a b) = "(a:" ++ show a ++ " " ++ "a:" ++ show b ++ ")"


-- | Nominal Terms Data Structure
data Term atm cst var   = Leaf (Leaf atm cst var)                           -- ^ a leaf
                        | Pair (Term atm cst var)       (Term atm cst var)  -- ^ a pair
                        | Modal (Modal atm)             (Term atm cst var)  -- ^ a modal
  deriving (Eq,Ord)


instance (Show atm, Show cst, Show var) => Show (Term atm cst var) where
 show (Leaf    l) = show l
 show (Pair  t u) = "("  ++ show t ++ "," ++ show u ++ ")"
 show (Modal m t) = show m ++ show t


-- ** Term Construction

atm :: atm -> Term atm cst var
atm = Leaf . Atm

cst :: cst -> Term atm cst var
cst = Leaf . Cst

var :: var -> Term atm cst var
var = Leaf . Var

swap :: atm -> atm -> Term atm cst var -> Term atm cst var
swap a b = Modal (Swap a b)

nomAbs :: atm -> Term atm cst var -> Term atm cst var
nomAbs a = Modal (Abs    a)

pair :: Term atm cst var -> Term atm cst var -> Term atm cst var
pair = Pair

-- ** Parsing functions
parse'atm :: (ParsecRead atm) => GenParser Char st (Leaf atm cst var)
parse'atm = (string "a:" >> parsecRead >>= return . Atm ) <?> "atom"

parse'cst :: (ParsecRead cst) => GenParser Char st (Leaf atm cst var)
parse'cst = (string "c:" >> parsecRead >>= return . Cst ) <?> "constant"

parse'var :: (ParsecRead var) => GenParser Char st (Leaf atm cst var)
parse'var = (string "v:" >> parsecRead >>= return . Var ) <?> "variable"

parse'leaf :: (ParsecRead atm, ParsecRead cst, ParsecRead var) =>
                   GenParser Char st (Leaf atm cst var)
parse'leaf = (parse'atm <|> parse'var <|> parse'cst) <?> "leaf"


instance (ParsecRead atm, ParsecRead cst, ParsecRead var) => ParsecRead (Leaf atm cst var) where
  parsecRead = parse'leaf


parse'abs :: (ParsecRead atm) => GenParser Char st (Modal atm)
parse'abs = (do char '[' >> spaces
                a <- string "a:" >> parsecRead
                spaces
                char ']'
                return $ Abs a
            ) <?> "abstraction"


parse'swap :: (ParsecRead atm) => GenParser Char st (Modal atm)
parse'swap =  (do char '(' >> spaces
                  a <- string "a:" >> parsecRead
                  spaces
                  b <- string "a:" >> parsecRead
                  spaces
                  char ')'
                  return $ Swap a b
              ) <?> "swapping"

parse'modal :: (ParsecRead atm) => GenParser Char st (Modal atm)
parse'modal = (parse'abs <|> parse'swap) <?> "Modal"


instance (ParsecRead atm) => ParsecRead (Modal atm) where
  parsecRead = parse'modal


parse'tmodal :: (ParsecRead atm, ParsecRead cst, ParsecRead var) =>
                   GenParser Char st (Term atm cst var)
parse'tmodal = (do m <- parse'modal
                   spaces
                   Modal m <$> parse'term
               ) <?> "term-modal"

parse'pair :: (ParsecRead atm, ParsecRead cst, ParsecRead var) =>
            GenParser Char st (Term atm cst var)
parse'pair =  (do char '('
                  spaces
                  t <- parse'term
                  spaces >> char ',' >> spaces
                  u <- parse'term
                  spaces
                  char ')'
                  return $ Pair t u
              ) <?> "Pair"

parse'term :: (ParsecRead atm, ParsecRead cst, ParsecRead var) =>
                     GenParser Char st (Term atm cst var)
parse'term =     (Leaf <$> parse'leaf)
             <|> try parse'tmodal
             <|> parse'pair


instance (ParsecRead atm, ParsecRead cst, ParsecRead var) => ParsecRead (Term atm cst var) where
  parsecRead = parse'term


-- instance (Read atm, Read cst, Read var) => Read (Leaf atm cst var) where
--   readsPrec _ = parse'read parse'leaf
-- 
-- instance (Read atm) => Read (Modal atm) where
--   readsPrec _ = parse'read parse'modal
-- 
-- instance (Read atm, Read cst, Read var) => Read (Term atm cst var) where
--   readsPrec _ = parse'read parse'term


-- | "substitute f t" substitute any variable v by (f v) in t. f is a monadic function
substituteM :: (Monad m) =>
          (var -> m (Maybe (Term atm cst var))) -> Term atm cst var -> m (Term atm cst var)
substituteM f = substM'
  where f' v = fromMaybe (var v) <$> f v

        substM' (Leaf (Var v)) = f' v
        substM' (Modal m t   ) = Modal m <$> substM' t
        substM' (Pair  s t   ) = do s' <- substM' s
                                    t' <- substM' t
                                    return $ Pair s' t'
        substM'          t     = return t


-- | "substitute f t" substitute any variable v by (f v) in t. f is a pure function
substitute :: (var -> Maybe (Term atm cst var)) -> Term atm cst var -> Term atm cst var
substitute f = runIdentity . substituteM (Identity . f)
