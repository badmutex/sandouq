module Sandouq.Internal.Init where

import System.Directory
import System.FilePath

import Sandouq.Internal.Config.Global
import Sandouq.Internal.Config.Local


initDir root = 
    mapM_ (createDirectoryIfMissing True . combine root . show)

initHidden d = initDir (d </> root) hiddenDirs
initVisible d = initDir d visibleDirs


addToGlobalConfig dir = 
    let name = last . splitDirectories $ dir
        str = configString (Name name) ++ "\n" ++
              configString (Location name dir) ++ "\n"
    in do appendFile configFile str
    

initialize dir = do
  createDirectoryIfMissing True dir
  initHidden dir
  initVisible dir
--  addToGlobalConfig dir
