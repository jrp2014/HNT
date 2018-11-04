{-# LANGUAGE	DeriveDataTypeable, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
-- | This is the Zipper Module inspired by
-- | /Generic Zipper and its applications/ by Oleg Kiselyov and al.
-- | Available at <http://okmij.org/ftp/Computation/Continuations.html#zipper>

module Data.Zipper.Zipper
  where
import Data.Zipper.Direction

import Control.Monad
import Control.Monad.ContState

-- | Requests that can be sent to the zipper
data ZipReq term dir = Move (Dir dir) -- ^ Move the zipper to dir
                     | Update term   -- ^ Update (replace) the current subterm by term

-- | The Zipper Data Structure
data Zipper term dir m = Zipper { -- | The Path of the current subterm
                                  zip'path :: [dir]  ,
                                  -- | The current subterm
                                  zip'term :: term ,
                                  -- | The continuation. It takes a request and give another zipper
                                  zip'cont :: ZipReq term dir -> m (Zipper term dir m)
                                }


-- | Show a path ala Unix, the list is reversed !
showPath :: (Show a) => [a] -> [Char]
showPath l = "/" ++ (foldl (++) "" $ map (\d -> "/" ++ (show d)) $ reverse l)

instance (Show dir, Show term) => Show (Zipper term dir m) where
  show (Zipper p t _) =        "Zipper "
                      ++     " (term = " ++ (show t)
                      ++    ") (path = " ++ (showPath p)
                      ++    ")"



-- | Updates the current subterm of a zipper
zip'update :: (Monad m) => term -> Zipper term dir m -> m (Zipper term dir m)
zip'update t z = zip'cont z (Update t)

-- | checks if the focus is at the root
zip'isRoot :: (Monad m) => Zipper term dir m -> Bool
zip'isRoot z = null (zip'path z)

-- | moves the zipper to dir
zip'move :: (Monad m) => Dir dir -> Zipper term dir m -> m (Zipper term dir m)
zip'move d z = zip'cont z (Move d)

-- | makes a number of moves
zip'moves :: (Monad m) => [Dir dir] -> Zipper term dir m -> m (Zipper term dir m)
zip'moves = flip $ foldM $ flip zip'move

-- | moves to the root
zip'toRoot :: (Monad m) => Zipper term dir m -> m (Zipper term dir m)
zip'toRoot z = if zip'isRoot z
                then return z
                else zip'move Up z >>= zip'toRoot



-- | The class of term that supports this zipper
-- | The minimal implementation is possibleDirs
class (Eq dir) => ZipperAble term dir where
  -- | give all possibles branch to the current term
  -- | possibleDirs term = [(dir, (subterm , rebuild'function)) , ....]
  -- | such that subterm is the subterm of term on the direction dir
  -- | and term = rebuild'function subterm
  -- |
  -- | For example : possibleDirs (Leaf _)    = []
  -- |               possibleDirs (Pair s u)  = [ (PairLeft  , (s , (\x -> Pair  x u))),
  -- |                                            (PairRight , (u , (\x -> Pair  s x)))
  -- |                                          ]
  -- |               possibleDirs (Modal m s) = [ (ModalDown , (s , (\x -> Modal m x))) ]
  possibleDirs :: term -> [(dir , (term , (term -> term)))]


-- Layers : Cont
--        : Stack of Cmds
--        : Tail of Stack of Possdirs
--        : Head of Stack of Possdir
  -- | builds the zipper of a term
  zipper :: term -> Zipper term dir (CS r l m)
  zipper t = Zipper [] t (\req -> do z <- runContL $ runStackL $ runStackL $ runStackL (
                                           fix (loop []) t
                                         )
                                     zip'cont z req
                         )
    where fix f s = do putL $ possibleDirs s
                       inc  $ putL []
                       inc2 $ putL []
                       f s >>= fix f

          loop p s = do req <- getReq p s
                        case req of
                         Update u        -> putL (possibleDirs u) >> loop p u
                         Move  Up        -> return s
                         Move  Down      -> findOneDown (possibleDirs s) >> loop p s
                         Move (DownTo d) -> goIn d p s
                         Move  Next      -> do possdir <- getL
                                               oldposs <- inc  $ getL
                                               findNext (possdir : oldposs)
                                               loop p s
 
          goIn d p u = case lookup d (possibleDirs u) of
                         Nothing    -> return u
                         Just (s,f) -> do newpossdir <- getsL $ filter (\(x,_) -> x /= d)
                                          oldstack   <- inc  $ getL
                                          inc  $ pushStateL newpossdir
                                          putL $ possibleDirs s
                                          u' <- loop (d : p) s
                                          putL $ newpossdir
                                          inc  $ putL oldstack
                                          loop p $ f u'

          getReq p s = do x <- inc2 $ popStateL
                          case x of
                           Nothing  -> inc3 $ shiftL (\k -> return $ Zipper p s k)
                           Just   d -> return (Move d)


          findOneDown []          = return False
          findOneDown ((d,_) : _) = do inc2 $ appendStateL (DownTo d)
                                       return True

          findNext  []     = return False
          findNext (x : l) = do b <- findOneDown x
                                if b
                                 then return True
                                 else do inc2 $ appendStateL Up
                                         findNext l

          runStackL m = runStateL [] m >>= return . fst