module Sandouq.Document where

import Control.Applicative
import Data.Digest.Pure.SHA
import Data.List
import System.Directory
import System.FilePath
import System.Posix.Files

import Sandouq.Config

data Author = Author {
      firstname  :: String
    , middlename :: String
    , lastname   :: String
    } deriving Show
authorDir a = lastname a ++ ", " ++ firstname a ++ rest
    where rest = if middlename a == "" then "" else ". " ++ middlename a ++ "."

newtype Title = Title { title :: String } deriving Show
titleName = title

newtype Tag = Tag { tag :: String } deriving Show
tagDir = tag


data Document = Doc {
      docTitle :: Title
    , authors  :: [Author]
    , tags     :: [Tag]
    , hash     :: Digest
    , suffix   :: String
    } deriving Show
info d = (show $ docTitle d) ++ " by " ++ as ++ "; Tags: " ++ ts
    where as = concat                    . map show $ authors d                 
          ts = concat . intersperse ", " . map show $ tags d 


data Link = Link {
      origin :: FilePath
    , target :: FilePath
    } deriving Show

testDoc = Doc (Title "MyTitle") ([Author "C" "Badi'" "Wahid"]) ([Tag "test",Tag "test2"]) undefined "asdf"



link :: (FilePath -> FilePath -> IO ()) -- ^ Somthing like 'createLink' or 'createSymbolicLink'
     -> Maybe Link                      -- ^ Link to create
     -> IO ()
link _ Nothing = pure ()
link f (Just l) = do --putStrLn $ "Linking " ++ origin l ++ " -> " ++ target l
                     createDirectoryIfMissing True (takeDirectory . target $ l)
                     f (origin l) (target l)

softLink, hardLink :: Maybe Link -> IO ()
softLink = link createSymbolicLink
hardLink = link createLink

check :: Link -> IO (Maybe Link)
check l = do oe <- doesFileExist (origin l)
             te <- doesFileExist (target l)
             case oe && (not te) of
               False -> do notify (origin l, oe) (target l, te)
                           return Nothing
               True  -> pure (Just l)

notify :: (FilePath, Bool) -> (FilePath, Bool) -> IO ()
notify (o,oe) (t,te) = do
  case oe of
    False -> putStrLn $ o ++ " does not exist."
    _     -> pure ()
  case te of
    True  -> putStrLn $ t ++ " already exists."
    _     -> pure ()



add :: FilePath -- ^ 'canonicalizePath'ed path to the document to add
    -> Document -- ^ 'Document' to add
    -> FilePath -- ^ path to box
    -> IO ()
add f d box = do
  let blob    = box </> blobPath (hash d)
      as      = targetAuthorFiles (authors d)
      ts      = targetTagFiles    (tags    d)
      links   = map (Link blob) (ts ++ as)
  copyFile f blob
  putStrLn $ "Authors: " ++ show as
--   putStrLn $ "Copied " ++ f ++ " to " ++ blob ++ "."
  mapM check links >>= mapM_ softLink
      where targetAuthorFiles = targetFiles (authorsPath . authorDir)
            targetTagFiles    = targetFiles (tagsPath . tagDir)
            targetFiles f     = map (\a -> box </> f a </> getTitle <.> suffix d)
            getTitle          = titleName . docTitle $ d
