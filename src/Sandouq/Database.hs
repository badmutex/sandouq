module Sandouq.Database where

import Data.List
import qualified Data.Map as Map
import Database.HDBC

data Table = Applications
           | Authors
           | IndividualAuthor -- | Dummy
           | DocType
           | Documents
           | Tags
           | IndividualTag -- | Dummy
             deriving (Eq, Ord, Show)

data Column = ID           -- | ubiquitous
            | CMD          -- | Applications table
            | Description  -- | Applications table
            | FullName     -- | Authors table
            | FirstName    -- | individual Authors table
            | MiddleName   -- | individual Authors table
            | LastName     -- | individual Authors table
            | Type         -- | DocType table
            | AppID        -- | DocType table
            | Hash         -- | Documents and individual tag tables
            | Path         -- | Documents table
            | OriginalName -- | Documents table
            | DocTypeID    -- | Documents table
            | Tag          -- | Tags table
            | DocHash      -- | individual Tag table
              deriving (Eq, Ord, Show)

tables :: Map.Map Table [Column]
tables = Map.fromList [ (Applications, [ID, CMD, Description])
                      , (Authors, [FullName])
                      , (DocType, [ID, Type, AppID])
                      , (Documents, [Hash, Path, OriginalName, DocTypeID])
                      , (Tags, [Tag])
                      ]

dummyTables = Map.fromList [ (IndividualAuthor, [FirstName, MiddleName, LastName])
                           , (IndividualTag, [DocHash])
                           ]

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

makeTable tables columns conn table =
    let constraints = map (\c -> columns `get` c) $ tables `get` table
    in createTable conn (show table) constraints

makeTables tables columns conn = 
    let mkTable = makeTable tables columns conn
    in mapM_ mkTable $ Map.keys tables

get m k = case Map.lookup k m of
            Just v -> v
            _ -> error "element not in the map"

createTable conn name constraints = do
  run conn ("CREATE TABLE IF NOT EXISTS " ++ name
            ++ " (" ++
            (concat $ intersperse ", " constraints)
            ++ ")"
           ) []

-- lookupTag :: (IConnection c) => Table -> String -> c -> IO (Maybe String)
lookupTag table tag conn = do
  cols <- quickQuery' conn ("SELECT tag FROM " ++ (show table)) []
  return . find (\t -> t == tag) $ concat $ map (map fromSql) cols
    
createTag conn tag = do
  t <- lookupTag Tags tag conn
  case t of
    Just _  -> return ()
    Nothing -> do createTable conn tag ["id INTEGER NOT NULL", "hash TEXT NOT NULL"]
                  insertInto2 conn "tags" "tag" [toSql tag]
                  return ()

insertInto2 conn name col info =
    run conn ("INSERT INTO " ++ name ++ " (" ++ col ++ ") VALUES (?)") info