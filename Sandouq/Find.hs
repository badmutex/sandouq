module Main where

import Control.Applicative
import Data.Char
import Data.List (intersperse)
import Data.Maybe
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString.Lazy.Char8 as BS

import Sandouq.Document
import Sandouq.Config


{- ******************** Functions ******************** -}

findHash :: FilePath -> Hash -> IO (Maybe FilePath)
findHash b h = let p =  b </> blobDir </> fromHash h
               in do fe <- doesFileExist p
                     pure $ if fe then Just p else Nothing

searchByHash :: FilePath -> FilePath -> IO (Maybe FilePath)
searchByHash b f = doHash f >>= findHash b

showSearchResult :: Maybe FilePath -> IO (Maybe Document)
showSearchResult Nothing = pure Nothing
showSearchResult p'@(Just p) = let h = takeFileName p
                                   d = (takeDirectory . takeDirectory) p </> show Docs </> h
                               in do d' <- (read . BS.unpack) `fmap` (BS.readFile d)
                                     putStrLn $ prettyDocument d'

                                     pure . Just $ d'

--putStrLn p >> pure p'


{- ******************** Program ******************** -}

data Options = Options {
      box        :: FilePath
    , document   :: Maybe FilePath
    , findByHash :: Bool
    , help       :: Bool
    } deriving Show

defaultOptions = Options {
                   box        = defaultBox
                 , document   = Nothing
                 , findByHash = False
                 , help       = False
                 }

options =
    [ Option "b" ["box"] (ReqArg          (\b opts -> opts {box = b}) "path/to/box")
                 ("Path to the box. Defaults to " ++ defaultBox)
    , Option "d" ["document"] (ReqArg     (\d opts -> opts {document = Just d}) "path/to/document")
                 "Path to the document"
    , Option "H" ["hash-and-find"] (NoArg (\opts -> opts {findByHash = True}))
                 "Hash the document and look in the box for it"
    , Option "h" ["help"] (NoArg          (\opts -> opts {help = True}))
                 "Show usage information"
    ]


showUsage = flip (++) (usageInfo ("\n" ++ header) options) . concat . intersperse ", "
    where header = "Usage: " ++ me ++ " OPTIONS"
          me     = unsafePerformIO getProgName

printUsage = putStrLn . showUsage

parseArgs :: [String] -> IO (Options, [String])
parseArgs args =
    case getOpt Permute options args of
      (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
      (_,_,errs) -> printUsage errs >> exitFailure



clean opts = do cleanedBox <- canonicalizePath (box opts)
                let cleanedDoc = (unsafePerformIO . canonicalizePath) `fmap` (document opts)
                pure . Just $ opts { box      = cleanedBox
                                   , document = cleanedDoc }

run args = 
    parseArgs args >>=
    pure . fst     >>=
    clean          >>=
    run'


run' Nothing = pure Nothing
run' (Just opts) = maybeShowHelp (help opts) >>
                   maybeHash (box opts) (findByHash opts) (document opts)
    where maybeShowHelp False = pure ()
          maybeShowHelp True = printUsage []

          maybeHash _ _     Nothing = pure Nothing
          maybeHash _ False _       = pure Nothing
          maybeHash b True (Just d) = searchByHash b d >>= showSearchResult


main = getArgs >>= run