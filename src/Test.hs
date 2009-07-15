module Main where

import Data.List
import Database.HDBC
import Database.HDBC.Sqlite3

conn = connectSqlite3 "/tmp/tags.db"

data Table = Tags deriving (Eq, Show)

main = do
  conn <- connectSqlite3 "/tmp/tags.db"

  run conn ("create table if not exists " 
            ++ (show Tags) ++ 
            " (id integer not null primary key asc autoincrement, tag text)"
           ) []
  new_table conn "foo" ["id INTEGER NOT NULL", "hash TEXT NOT NULL"]

  new_tag conn "foo"
  new_tag conn "magic"
  new_tag conn "hello"
  new_tag conn "world"

  commit conn

  rows <- quickQuery' conn "SELECT * FROM tags" []
  
  print rows

  disconnect conn
  
find_tag :: (IConnection c) => Table -> String -> c -> IO (Maybe String)
find_tag table tag conn = do
  cols <- quickQuery' conn ("SELECT tag FROM " ++ (show table)) []
  return . find (\t -> t == tag) $ concat $ map (map fromSql) cols
    
new_tag conn tag = do
  t <- find_tag Tags tag conn
  case t of
    Just _  -> return ()
    Nothing -> do new_table conn tag ["id INTEGER NOT NULL", "hash TEXT NOT NULL"]
                  insert_into2 conn "tags" "tag" [toSql tag]
                  return ()

new_table :: (IConnection c) => c -> String -> [String] -> IO Integer
new_table conn name constraints =
  run conn ("CREATE TABLE IF NOT EXISTS " ++ name
            ++ "(" ++
            (concat $ intersperse "," constraints)
            ++ ")"
           ) []

insert_into2 conn name col info =
    run conn ("INSERT INTO " ++ name ++ " (" ++ col ++ ") VALUES (?)") info