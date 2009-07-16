module Sandouq.Init where

import System.Directory
import System.FilePath

import Sandouq.Config
import Sandouq.Database

usage = exec_cmd ++ " <database directory>"

maybeMakeDir :: FilePath -> IO ()
maybeMakeDir path =
    doesDirectoryExist path >>=
    \e -> case e of
            False -> createDirectory path
            True  -> return ()
  

initDatabase dbconn path = do
  conn <- connect dbconn
          
  makeInitialTables newDatabase conn
  commit conn

  return conn
  
  

makeDatabase config_file db_path = do
--   add_to_init config_file db_path db_name
  maybeMakeDir db_path
  let db_file = combine db_path sqlite3_db_file_name
  initDatabase (mkSqlite3DBConn db_file) db_file
