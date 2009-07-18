module Main where

import Control.Monad.Instances
import System.Directory
import System.Environment

import Sandouq.Internal.Init

usage = " <directory>"

printUsage = getProgName >>= print . flip (++) usage

getArgs' :: IO (Maybe FilePath)
getArgs' = do
  args <- getArgs
  if length args /= 1 then printUsage >> return Nothing
     else canonicalizePath (head args) >>= return . Just


main = do
  dir <- getArgs'
  case dir of
    Nothing -> return ()
    Just d  -> print ("Initializing " ++ d) >> initialize d
