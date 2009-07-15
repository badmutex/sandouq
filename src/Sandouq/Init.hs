module Sandouq.Init where

import qualified Data.Map as Map
import Database.HDBC
import System.Directory
import System.FilePath.Posix

import Sandouq.Config
import Sandouq.Database

usage = exec_cmd ++ " <database directory> <database name>"

maybeMakeDir path =
    doesDirectoryExist path >>=
    \e -> case e of
            False -> createDirectory path
            True  -> return ()
  

initDatabase path = do
  conn <- connectToDatabase path
          
  

  return conn
  
  

makeDatabase config_file db_path = do
--   add_to_init config_file db_path db_name
  maybeMakeDir $ dropFileName db_path
  initDatabase db_path