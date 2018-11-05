-- | Freshness context API module
module Data.Nominal.FrsCtxt where


import Text.ParserCombinators.Parsec

import qualified Data.Set as Set
import qualified Data.Map as Map

import Hnt.Utils

newtype FrsCtxt v a = FrsCtxt { unFrsCtxt :: Map.Map v (Set.Set a) }
  deriving (Eq,Ord)

-- * Parsing function


instance (Show v, Show a) => Show (FrsCtxt v a) where
  show (FrsCtxt m) =
     printSeq "" "" " " $ map (\(v,s) -> showAtmSet s ++ "#v:" ++ show v) (Map.toList m)

showAtmSet :: (Show a) => Set.Set a -> String
showAtmSet s = printSeq "{" "}" " " (map (\a -> "a:" ++ show a) (Set.elems s))

parse'constr :: (ParsecRead a, ParsecRead b, Ord b) => GenParser Char st (a, Set.Set b)
parse'constr = ( do s <- parse'set (string "a:" >> parsecRead)
                    spaces >> char '#' >> spaces
                    v <- string "v:" >> parsecRead
                    return (v, Set.fromList s)
               ) <?> "Constaint"


parse'frsCtxt :: (Ord b, Ord a, ParsecRead a, ParsecRead b) =>
                 GenParser Char st (FrsCtxt a b)
parse'frsCtxt = do l <- parse'seq parse'constr
                   return $ FrsCtxt $ foldl f Map.empty l
  where f m (v,s) = Map.insertWith Set.union v s m


instance (Ord atm, Ord var, ParsecRead atm, ParsecRead var) => ParsecRead (FrsCtxt var atm) where
  parsecRead = parse'frsCtxt


-- * Functions

empty :: FrsCtxt v a
empty = FrsCtxt Map.empty

frsVarSet :: (Ord v, Ord a) => v -> FrsCtxt v a -> Set.Set a
frsVarSet v = Map.findWithDefault Set.empty v . unFrsCtxt

addFrsCtxt :: (Ord v, Ord a) => v -> Set.Set a -> FrsCtxt v a -> FrsCtxt v a
addFrsCtxt v s (FrsCtxt fc) = FrsCtxt $ Map.insertWith Set.union v s fc

mergeFrsCtxt :: (Ord v, Ord a) => FrsCtxt v a -> FrsCtxt v a -> FrsCtxt v a
mergeFrsCtxt (FrsCtxt fc1) (FrsCtxt fc2) = FrsCtxt $ Map.unionWith Set.union fc1 fc2

isInFrsCtxt :: (Ord v, Ord a) => v -> Set.Set  a -> FrsCtxt v a -> Bool
isInFrsCtxt v s fc = Set.isSubsetOf s $ frsVarSet v fc

isSubfrsCtxtOf :: (Ord v, Ord a) => FrsCtxt v a -> FrsCtxt v a -> Bool
isSubfrsCtxtOf (FrsCtxt fc1) (FrsCtxt fc2) = Map.isSubmapOfBy Set.isSubsetOf fc1 fc2
