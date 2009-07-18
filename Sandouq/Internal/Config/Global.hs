module Sandouq.Internal.Config.Global where

import qualified Data.List as L
--import Prelude ((++), String, ($), fmap, lines, readFile, Eq, Show, Ord, (.), flip)
import Data.List (intersperse)
import Control.Monad
import Data.Map
import System.Directory
import System.FilePath
import System.IO.Unsafe

import Sandouq.Internal.Tools

cmd = "sd"

{-# NOINLINE configFile #-}
configFile :: FilePath
configFile = unsafePerformIO $
             getHomeDirectory >>= \d -> return $ d </> ".sandouq"


data ConfigOption = Name String
                  | Location String FilePath
                    deriving (Eq, Ord, Show)

data Box = Box {
      name :: Maybe String
    , location :: Maybe String
    } deriving (Eq, Show)

newBox = Box Nothing Nothing

box :: Box
box = undefined

boxes :: [Box]
boxes = undefined



{-# NOINLINE configFileData #-}
configFileData :: [[String]]
configFileData = unsafePerformIO $
                 fmap (L.map words . lines) (readFile configFile)
                 

configString :: ConfigOption -> String
configString (Name s) = "box.name " ++ s
configString (Location n l) = n ++ ".location " ++ l
