module Main where

import Control.Applicative
import Control.Monad.Instances
import System.Directory
import System.Environment

import Sandouq.Internal.Documents.Add

usage = "<path to bx> <document to add>"

printUsage = getProgName >>= print . flip (++) usage

data Document = Doc {
      box :: FilePath
    , document :: FilePath
    } deriving Show

getArgs' :: IO (Maybe Document)
getArgs' = do
  args <- getArgs
  if length args /= 2 then printUsage >> return Nothing
     else do
       b <- canonicalizePath (args !! 0)
       d <- canonicalizePath (args !! 1)
       return $ Just (Doc b d)

main = do
  b <- getArgs'
  case b of
    Nothing -> return ()
    Just b  -> add (box b) (document b)

main' = do
  let file = "~/Documents/resume/resume.pdf"
      title = "Badi's Resume"

  t <- promptForTitle file
  (putStrLn . show) t --`fmap` promptForTitle file

  ts <- promptForTags t
  (putStrLn . show) ts --`fmap` promptForTags title

  as <- promptForAuthors file
  (putStrLn . show) as --`fmap` promptForAuthors file
