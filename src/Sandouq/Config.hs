module Sandouq.Config where

import Database.HDBC
import Database.HDBC.Sqlite3

exec_cmd = "sd"
config_file = "~/.sandouqrc"
connectToDatabase = connectSqlite3