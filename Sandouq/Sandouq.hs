module Main where

import Control.Applicative
import Control.Monad.Instances
import Data.List
import Data.Maybe
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.Directory
import System.FilePath

import Sandouq.Internal.Init
import Sandouq.Internal.Documents.Add

data Flag = InitBox
          | AddDocument FilePath
          | Box FilePath
            deriving (Eq, Show)

getBox (Box b) = b
getDoc (AddDocument d) = d

options :: [OptDescr Flag]
options = [ Option ['i'] ["initialize"] (NoArg InitBox)                    "create the box"
          , Option ['b'] ["box"]        (ReqArg Box "path/to/box")         "path to the box"
          , Option ['a'] ["document"]   (ReqArg AddDocument "path/to/doc") "path to the document to add"
          ]


header = "Usage: sd -i -b <path> | -a <path> -b <path>"
fail' = putStrLn (usageInfo header options) >> exitFailure
getArgs' argv = 
    case getOpt Permute options argv of
      ([],[],[]) -> fail'
      (o,n,[])   -> return o
      (_,_,errs) -> fail'


initBox = find (\x -> case x of
                        InitBox -> True
                        _       -> False)

box = find (\x -> case x of
                    Box p -> True
                    _     -> False)

doc = find (\x -> case x of
                    AddDocument p -> True
                    _             -> False)


init' b = putStrLn ("Initializing " ++ b) >> canonicalizePath b >>= initialize
addDoc d b = do b' <- canonicalizePath b
                d' <- canonicalizePath d
                putStrLn ("Adding " ++ d' ++ " into " ++ b')
                add b' d'

run args =
    case box args of
      Nothing -> fail'
      Just b  -> if isJust (initBox args) then init' (getBox b)
                 else if isJust (doc args) then addDoc (getDoc . fromJust $ doc args) (getBox b)
                      else fail'

main = getArgs >>= getArgs' >>= run >> exitSuccess
