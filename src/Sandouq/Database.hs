{-# LANGUAGE FlexibleContexts #-}

module Sandouq.Database (
                         -- * All 'Database.HDBC' modules
                         module Database.HDBC

                        -- * Data structures
                        , Table(..)
                        , Column(..)
                        , DatabaseConnector(..)

                        -- * Variables and Functions
                        -- ** Variables
                        , tables
                        , dummyTables
                        , columns

                        -- ** Functions
                        , makeTable
                        , makeTables
                        , createTable
                        , lookupTag
                        , createTag
                        , insertInto1
                        , mkSqlite3DBConn

                        ) where

import Data.Convertible
import Data.List
import qualified Data.Map as Map
import Database.HDBC
import Database.HDBC.Sqlite3

data Table = Applications
           | Authors
           | IndividualAuthor -- ^ Dummy table. Holds the template for field types for the individual authors
           | DocType
           | Documents
           | Tags
           | IndividualTag -- ^ Dummy table. Holds the template for field types for individual tages.
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

-- | Maps the tables to their columns. Used to create a new database.
tables :: Map.Map Table [Column]
tables = Map.fromList [ (Applications, [ID, CMD, Description])
                      , (Authors,      [FullName])
                      , (DocType,      [ID, Type, AppID])
                      , (Documents,    [Hash, Path, OriginalName, DocTypeID])
                      , (Tags,         [Tag])
                      ]

-- | Maps the dummy tables to their columns. Used for creating the individual tables.
dummyTables :: Map.Map Table [Column]
dummyTables = Map.fromList [ (IndividualAuthor, [FirstName, MiddleName, LastName])
                           , (IndividualTag, [DocHash])
                           ]


-- | Maps the columns to the SQL string used to create the column.
columns :: Map.Map Column String
columns = Map.fromList [ (ID,          (show ID) ++ " INTEGER NOT NULL PRIMARY KEY ASC AUTOINCREMENT")
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

-- | Generates the constraints for the table table and create it. Does not commit to the database.
makeTable :: IConnection conn => Map.Map Table [Column] -> Map.Map Column String -> conn -> Table -> IO ()
makeTable tables columns conn table =
    let constraints = map (\c -> columns `get` c) $ tables `get` table
    in createTable conn (show table) constraints >> return ()

-- | Calls 'makeTable' on each of the keys in the tables map. Does not commit to the database.
makeTables :: (IConnection conn) => Map.Map Table [Column] -> Map.Map Column String -> conn -> IO ()
makeTables tables columns conn = 
    let mkTable = makeTable tables columns conn
    in mapM_ mkTable $ Map.keys tables

-- | I'm qualifying the import of 'Data.Map' which prevents me from using '(!)', so this serves as a replacement.
get :: (Ord k) => Map.Map k a -> k -> a
get m k = case Map.lookup k m of
            Just v -> v
            _ -> error "element not in the map"

-- | Creates a table using the given name and constraints. Does not commit to the database.
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
    
-- | Creates a new tag in the database if it does not exist, else nothing. Does not commit to the database.
createTag :: (IConnection conn) => conn -> String -> IO ()
createTag conn tag = do
  t <- lookupTag Tags tag conn
  case t of
    Just _  -> return ()
    Nothing -> do createTable conn tag ["id INTEGER NOT NULL", "hash TEXT NOT NULL"]
                  insertInto1 conn "tags" "tag" [toSql tag]
                  return ()

-- | Inserts into the table name at the column the information. Does not commit to the database.
insertInto1 :: (IConnection conn) =>
               conn -> String -> String -> [SqlValue] -> IO Integer
insertInto1 conn name col info =
    run conn ("INSERT INTO " ++ name ++ " (" ++ col ++ ") VALUES (?)") info



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
