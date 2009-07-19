module Sandouq.Internal.Config.Global where

import System.Directory
import System.FilePath
import System.IO.Unsafe

import Sandouq.Internal.Tools

defaultBox = (unsafePerformIO getHomeDirectory) </> "Documents" </> "Sandouq"


