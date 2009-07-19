module Sandouq.Config where

import qualified Data.ByteString.Lazy as BS
import Data.Digest.Pure.SHA
import System.Directory
import System.FilePath
import System.IO.Unsafe

import Sandouq.Tools


root = ".sandouq"

data InfoDir = Authors
             | Blobs
             | Tags
               deriving Show

hiddenDirs = [Blobs]
visibleDirs = [Authors, Tags]

blobPath :: Digest -> FilePath
blobPath digest = root </> show Blobs </> show digest

authorsPath :: String -> FilePath
authorsPath = getVisiblePath Authors

tagsPath :: String -> FilePath
tagsPath = getVisiblePath Tags

getHiddenPath :: InfoDir -> String -> FilePath
getHiddenPath i s = root </> show i </> s

getVisiblePath :: InfoDir -> String -> FilePath
getVisiblePath i s = show i </> s


defaultBox = (unsafePerformIO getHomeDirectory) </> "Documents" </> "Sandouq"
