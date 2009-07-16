{-# LANGUAGE FlexibleContexts #-}

module Sandouq.Database (
                         -- * All 'Database.HDBC' modules
                         module Database.HDBC

                        -- * Data structures
                        , Database(..)
                        , Table(..)
                        , Column(..)
                        , DatabaseConnector(..)

                        -- * Constants and Functions
                        -- ** Constants
                        , newDatabase

                        -- ** Functions
                        , constraints
                        , makeTable
                        , addTable
                        , makeInitialTables
                        , createTable
                        , lookupTag
                        , createTag
                        , insertInto
                        , mkSqlite3DBConn

                        ) where

import Data.Convertible
import Data.List
import qualified Data.Map as Map
import Database.HDBC
import Database.HDBC.Sqlite3

data Table = Applications
           | Authors
           | DummyAuthor -- ^ Template for the author tables
           | DocType
           | Documents
           | Tags
           | DummyTag    -- ^ Template for the tag tables
             deriving (Eq, Ord, Show)

data Column = ID           -- ^ ubiquitous
            | CMD          -- ^ Applications table
            | Description  -- ^ Applications table
            | FullName     -- ^ Authors table
            | FirstName    -- ^ individual Authors table
            | MiddleName   -- ^ individual Authors table
            | LastName     -- ^ individual Authors table
            | Type         -- ^ DocType table
            | AppID        -- ^ DocType table
            | Hash         -- ^ Documents and individual tag tables
            | Path         -- ^ Documents table
            | OriginalName -- ^ Documents table
            | DocTypeID    -- ^ Documents table
            | Tag          -- ^ Tags table
            | DocHash      -- ^ individual Tag table
              deriving (Eq, Ord, Show)


data Database = DB {
      tables      :: Map.Map Table [Column] -- ^ Maps the tables to their columns.
    , dummyTables :: Map.Map Table [Column] -- ^ Maps the dummy tables to their columns.
    , columns     :: Map.Map Column String  -- ^ Maps the columns to the SQL string used to create the column.
    , newTables   :: Map.Map String Table   -- ^ New tables, templated on 'DummyAuthor' or 'DummyTag'
    } deriving Show

newDatabase :: Database
newDatabase = DB tables' dummyTables' columns' Map.empty

tables' = Map.fromList [ (Applications, [ID, CMD, Description])
                      , (Authors,      [FullName])
                      , (DocType,      [ID, Type, AppID])
                      , (Documents,    [Hash, Path, OriginalName, DocTypeID])
                      , (Tags,         [Tag])
                      ]

dummyTables' = Map.fromList [ (DummyAuthor, [FirstName, MiddleName, LastName])
                           , (DummyTag,     [DocHash])
                           ]

columns' = Map.fromList [ (ID,         (show ID) ++ " INTEGER NOT NULL PRIMARY KEY ASC AUTOINCREMENT")
                       , (CMD,         (show CMD) ++ " TEXT NOT NULL")
                       , (Description, (show Description) ++ " TEXT")
                       , (FullName,    (show FullName) ++ " TEXT NOT NULL PRIMARY KEY")
                       , (FirstName,   (show FirstName) ++ " TEXT NOT NULL")
                       , (MiddleName,  (show MiddleName) ++ " TEXT NOT NULL")
                       , (LastName,    (show LastName) ++ " TEXT NOT NULL")
                       , (Type,        (show Type) ++ " TEXT NOT NULL")
                       , (AppID,       (show AppID) ++ " INTEGER NOT NULL " 
                          ++ "CONSTRAINT " ++ (show AppID) 
                          ++ " REFERENCES " ++ (show Applications) ++ " (" ++ (show ID) ++ ")")
                       , (Hash,        (show Hash) ++ " TEXT NOT NULL PRIMARY KEY")
                       , (Path,        (show Path) ++ " TEXT NOT NULL")
                       , (OriginalName,(show OriginalName) ++ " TEXT NOT NULL")
                       , (DocTypeID,   (show DocTypeID) ++ " INTEGER NOT NULL " 
                          ++ "CONSTRAINT " ++ (show DocTypeID)
                          ++ " REFERENCES " ++ (show DocType) ++ " (" ++ (show ID) ++ ")")
                       , (Tag,         (show Tag) ++ " TEXT NOT NULL")
                       , (DocHash,     (show DocHash) ++ " TEXT NOT NULL " 
                          ++ "CONSTRAINT " ++ (show DocHash)
                          ++ " REFERENCES " ++ (show Documents) ++ " (" ++ (show Hash) ++ ")")
                       ]


-- | Generates the constraints for the table table and create it. Does not commit.
makeTable :: IConnection c => Database -> c -> Table -> IO ()
makeTable db c t =
    let cs = constraints db tables t
    in createTable c (show t) cs >> return ()

-- | Addes a new table to the database. Does not commit.
addTable :: IConnection conn =>
            Database
         -> conn
         -> String -- ^ new table name
         -> Table  -- ^ dummy table
         -> IO Database
addTable db conn table dummy =
    let cs = constraints db dummyTables dummy
    in do createTable conn table cs
          return $ db {newTables = Map.insert table dummy (newTables db)}

-- | Gets the constraints for a table.
constraints :: (Ord k) =>
               Database
            -> (Database -> Map.Map k [Column]) -- ^ either 'tables' or 'dummyTables'
            -> k
            -> [String]
constraints db tsf t = map (columns db `get`) $ tsf db `get` t


-- | Calls 'makeTable' on each of the keys in the tables map. Does not commit.
makeInitialTables :: (IConnection conn) => Database -> conn -> IO ()
makeInitialTables db conn =let mkTable = makeTable db conn
    in mapM_ mkTable . Map.keys $ tables db

-- | I'm qualifying the import of 'Data.Map' which prevents me from using '(!)', so this serves as a replacement.
get :: (Ord k) => Map.Map k a -> k -> a
get m k = case Map.lookup k m of
            Just v -> v
            _ -> error "element not in the map"

-- | Creates a table using the given name and constraints. Does not commit.
createTable :: (IConnection conn) => conn -> String -> [String] -> IO Integer
createTable conn name constraints = do
  run conn ("CREATE TABLE IF NOT EXISTS " ++ name
            ++ " (" ++
            (concat $ intersperse ", " constraints)
            ++ ")"
           ) []

-- | Attempts to return a tag if it exists in the given table.
lookupTag :: IConnection conn => Table -> String -> conn -> IO (Maybe String)
lookupTag table tag conn = do
  cols <- quickQuery' conn ("SELECT tag FROM " ++ (show table)) []
  return . find (\t -> t == tag) $ concat $ map (map fromSql) cols
    
-- | Creates a new tag in the database if it does not exist, else nothing. Does not commit.
createTag :: (IConnection conn) => conn -> String -> IO ()
createTag conn tag = do
  t <- lookupTag Tags tag conn
  case t of
    Just _  -> return ()
    Nothing -> do createTable conn tag ["id INTEGER NOT NULL", "hash TEXT NOT NULL"]
                  insertInto conn Tags $ zip [Tag] [toSql tag]
                  return ()

-- | Inserts the values into the table at specified columns.  Does not commit.
-- The @[Column]@ and @[SqlValue]@ should have the same length, otherwise error.
insertInto :: (IConnection conn) => conn -> Table -> [(Column, SqlValue)] -> IO ()
insertInto conn table info =
    let (cols,vals) = unzip info
        flatten = concat . intersperse ", "
        cs          = flatten $ map show cols
        vs          = flatten $ map (\_ -> "?") cols
    in do run conn ("INSERT INTO " ++ show table ++ " (" ++ cs ++ ") VALUES (" ++ vs ++ ")") vals
          return ()


-- | Holds the information needed to connect to a database
data DatabaseConnector = DBConn {
      connect  :: IO Connection
    , username :: String
    , password :: String
    }

-- | Create a 'DatabaseConnector' to an Sqlite3 database
mkSqlite3DBConn :: FilePath -> DatabaseConnector
mkSqlite3DBConn path = DBConn {
        connect = connectSqlite3 path
      , username = ""
      , password = ""
      }
