module Sandouq.Config where


import System.Directory
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)

data HiddenDir = Blobs | Docs    deriving Show
data VisibleDir = Tags | Authors deriving Show

defaultBox = unsafePerformIO getHomeDirectory </> "Documents" </> "Sandouq"

hiddenRoot = ".sandouq"
blobDir = hiddenRoot </> show Blobs

hiddenDirs :: [HiddenDir]
hiddenDirs = [Blobs, Docs]

visibleDirs :: [VisibleDir]
visibleDirs = [Tags, Authors]


