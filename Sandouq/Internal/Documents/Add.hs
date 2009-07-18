module Sandouq.Internal.Documents.Add where

import Control.Applicative
import Control.Monad.Instances
import qualified Data.ByteString.Lazy as BS
import Data.Digest.Pure.SHA
import System.Directory
import System.FilePath
import System.Posix.Files
import System.IO

import Sandouq.Internal.Config.Local

hash :: FilePath -> IO Digest
hash = fmap (fmap sha1) BS.readFile

addBlob :: FilePath -> FilePath -> IO ()
addBlob original new = copyFile original new

linkTo :: (String -> FilePath)  -- ^ function to get the path to the parent directory
       -> Digest -- ^ hash of the file
       -> String -- ^ title
       -> String -- ^ suffix
       -> [String] -- ^ directories
       -> IO ()
linkTo getParent digest title suffix dirs = do
  let origin  = blobPath digest
      targets = map (\n -> getParent n </> title <.> suffix) dirs
  mapM_ (\t -> do createDirectoryIfMissing True (takeDirectory t)
                  createLink origin t) targets

linkToAuthors :: Digest -> String -> String -> [String] -> IO ()
linkToAuthors = linkTo authorsPath

linkToTags :: Digest -> String -> String -> [String] -> IO ()
linkToTags = linkTo tagsPath

promptForAuthors :: FilePath -> IO [String]
promptForAuthors file = do
  putStrLn $ "Enter the author names for " ++ takeFileName file ++ "."
  putStrLn $ "Press <Ctrl-D> to finish"
  l <- getContents
  return $ lines l


promptForTitle :: FilePath -> IO String
promptForTitle file = do
  putStrLn $ "What is the title of " ++ takeFileName file ++ "."
  l <- getLine
  return l


promptForTags :: String -> IO [String]
promptForTags title = do
  putStrLn $ "Enter a space-separated list of tags for " ++ title ++ "."
  l <- getLine
  return $ words l


add :: FilePath  -- ^ path to the Box directory
    -> FilePath  -- ^ path to the file to be added
    -> IO ()
add box file = do
  digest  <- hash file
  title   <- promptForTitle file

  cwd     <- getCurrentDirectory
  add' box file digest title
  setCurrentDirectory cwd

add' :: FilePath -> FilePath -> Digest -> String -> IO ()
add' box file digest title = do
  addBlob file (box </> blobPath digest)
  setCurrentDirectory box
  
  promptForTags title   >>= linkToTags digest title (takeExtension file)
  promptForAuthors file >>= linkToAuthors digest title (takeExtension file)
