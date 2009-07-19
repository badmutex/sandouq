module Main where

import Control.Applicative
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.FilePath
import System.IO.Unsafe

import Sandouq.Internal.Config.Global
import Sandouq.Internal.Config.Local

me = unsafePerformIO getProgName

data Options = Options {
      box     :: FilePath
    } deriving Show


defaultOptions = Options {
                   box = defaultBox
                 }

options =
    [ Option ['i'] ["initialize"]
                 (ReqArg (\b opts -> opts {box = b}) "path/to/box")
                 ("Path to the box. Defaults to " ++ defaultBox ++ ".")
    ]

parseArgs :: [String] -> IO (Options, [String])
parseArgs argv =
    case getOpt Permute options argv of
      (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
        where header = "Usage: " ++ me ++ " -i PATH"


clean :: Options -> IO (Maybe Options)
clean opts = do
  cleanedBox <- canonicalizePath $ box opts
  pure . Just $ opts { box = cleanedBox }

genArgs Nothing     = return Nothing
genArgs (Just opts) = return . Just $ box opts

main =
  getArgs    >>=
  parseArgs  >>=
  pure . fst >>=
  clean      >>=
  genArgs    >>=
  run
      where run Nothing = pure ()
            run (Just p) = initialize p

initialize dir = do
  createDirectoryIfMissing True dir
  initHidden dir
  initVisible dir

initHidden d = initDir (d </> root) hiddenDirs
initVisible d = initDir d visibleDirs

initDir root = 
    mapM_ (createDirectoryIfMissing True . combine root . show)


data ConfigOption = Name String
                  | Location String FilePath
                    deriving (Eq, Ord, Show)

configString :: ConfigOption -> String
configString (Name s) = "box.name " ++ s
configString (Location n l) = n ++ ".location " ++ l