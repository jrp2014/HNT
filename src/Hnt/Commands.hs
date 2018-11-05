module Hnt.Commands where

import Data.Nominal.Term

import Text.ParserCombinators.Parsec
import Hnt.Utils

import Nominal.Rewriting

import qualified Data.Nominal.FrsCtxt as FC

import Control.Monad.Trans.Except
import Control.Monad.State

import qualified Data.Zipper as Z
import Control.Monad.Zipper


import Control.Monad.ContState

import System.IO


help_string = "\
\\nSyntax\
\\n======\
\\n\
\\n In the following, X represent an Integer, t, s and u three terms.\
\\n\
\\nTerms\
\\n-----\
\\n\
\\n A term is :\
\\n\
\\n a:X         : an atom\
\\n c:X         : a constant\
\\n v:X         : a variable\
\\n (a:X a:X) t : a swapping\
\\n [a:X] t     : an abstraction\
\\n (t , u)     : a pair\
\\n\
\\nFreshness Context, Rewriting Rule and Term in Context\
\\n-----------------------------------------------------\
\\n\
\\n A freshness context fc is {a:X ... a:X}#v:X ... {a:X .... a:X}#v:X\
\\n A rewriting rule rl is fc |- t -> s\
\\n A term in context tc is fc |- t\
\\n\
\\n \
\\nCommands\
\\n========\
\\n\
\\n help               : print an help message\
\\n move down          : move to the first subterm\
\\n move down to left  : move to the left subterm\
\\n move down to right : move to the right subterm\
\\n move down to modal : move to the modal subterm\
\\n move up            : move up (yes!)\
\\n move down          : move to the next unvisited position\
\\n update t           : replace the current subterm by t\
\\n rewrite rl         : apply the rule rl to the current subterm\
\\n context fc         : replace the freshness context by fc\
\n\n"



{-----------
- COMMANDS -
-----------}

data ZipCmd = Move    (Z.Dir TermDir)
            | ToRoot
            | Update  (Term Integer Integer Integer)
            | Rewrite (Rule Integer Integer Integer)
  deriving (Eq,Ord,Show)


instance ParsecRead ZipCmd where
  parsecRead = (    (string "toroot" >> return ToRoot)
                <|> (string "move"    >> space >> parsecRead >>= return . Move   )
                <|> (string "update"  >> space >> parsecRead >>= return . Update )
                <|> (string "rewrite" >> space >> parsecRead >>= return . Rewrite)
               ) <?> "update, move, update or rewrite"


data ItfCmd = Help | Context (FC.FrsCtxt Integer Integer)
  deriving (Eq,Ord,Show)
  

instance ParsecRead ItfCmd where
  parsecRead = (    (string "help"    >> return Help)
                <|> (string "context" >> spaces >> parsecRead >>= return . Context)
               ) <?> "interface command, type help"


data Cmd = ItfCmd ItfCmd | ZipCmd ZipCmd
  deriving (Eq,Ord,Show)


instance ParsecRead Cmd where
  parsecRead = (    (parsecRead >>= return . ItfCmd)
                <|> (parsecRead >>= return . ZipCmd)
               ) <?> "command"



{-----------------------
- COMMAND INPUT/OUTPUT -
-----------------------}

output :: String -> CS r l IO ()
output s = liftCS $ putStr s >> hFlush stdout 

input :: CS r l IO String
input    = liftCS $ getLine

prompt :: (ParsecRead a) => CS r l IO String -> CS r l IO a
prompt m = eitherUntilM (\e -> output ( "\nSyntax Error at " ++ (show e)
                                       ++ "\nType help for help\n\n" )
                        )
                        (do m >>= output
                            input >>= return . (parseWithSpace parsecRead)
                        )

getZipCmd :: CS
               r
               (ExtB
                  (ExtB l e1 (StateT (FC.FrsCtxt Integer Integer)))
                  e
                  (StateT (Z.Zipper (Term Integer Integer Integer) TermDir m1)))
               IO
               ZipCmd
getZipCmd = do t <- prompt cmdPrompt 
               case t of
                ZipCmd c -> return c
                ItfCmd c -> case c of
                             Help       -> output help_string >> getZipCmd
                             Context fc -> (inc $ putL fc)    >> getZipCmd
 where
  cmdPrompt = do term <- zip'term
                 path <- zip'path
                 ctxt <- inc $ getL
                 return (    "\n  Path = " ++ (Z.showPath path)
                          ++ "\n  Term = " ++ (show term)
                          ++ "\n"
                          ++ "\n  Ctxt = " ++ (show ctxt)
                          ++ "\n\n=> "
                        )

{-------------------
- INTERACTIVE LOOP -
-------------------}


innerLoop :: ZipCmd
             -> CS
                  r
                  (ExtB
                     (ExtB (ExtB l e1 (ExceptT [Char])) e2 (StateT (FC.FrsCtxt Integer Integer)))
                     e
                     (StateT
                        (Z.Zipper
                           (Term Integer Integer Integer)
                           TermDir
                           (CS
                              r
                              (ExtB (ExtB l e1 (ExceptT [Char])) e2 (StateT (FC.FrsCtxt Integer Integer)))
                              IO))))
                  IO
                  ()
innerLoop (Move   dir) = zip'move dir
innerLoop (Update t  ) = zip'update t
innerLoop (ToRoot    ) = zip'toRoot
innerLoop (Rewrite rl) = zip'term >>= inc . (catchErrorRewrite rl) >>= zip'update
 where catchErrorRewrite rl t = shiftStateL (\s -> catchErrorL (runStateL s $ rewrite rl t)
                                                     (\e -> do output ("Rule didn't match : "
                                                                         ++ e ++ "\n"
                                                                      )
                                                               return (t,s)
                                                     )
                                            )
                                                              


loop :: IO ()
loop = (runCSIO $ runErrorL (
          do CtxtTerm ctxt term <- prompt $ return (help_string ++ "\nEnter Term in Context : ")
             runStateL ctxt $ runZipL term $ repeatM (getZipCmd >>= innerLoop)
         )
       ) >> return ()
