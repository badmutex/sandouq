module Main where

import Control.Applicative
import Data.Digest.Pure.SHA
import Data.Maybe (fromJust)
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.FilePath
import System.IO.Unsafe
import qualified Data.ByteString.Lazy as BS
import qualified Data.Set as S

import Sandouq.Config
import qualified Sandouq.Document as Doc

me = unsafePerformIO getProgName

data Options = Options {
      box     :: FilePath
    , doc     :: FilePath
    , title   :: Maybe String
    , authors :: Maybe [String]
    , tags    :: Maybe [String]
    } deriving Show


defaultOptions = Options {
                   box     = defaultBox
                 , doc     = ""
                 , title   = Nothing
                 , authors = Nothing
                 , tags    = Nothing
                 }


options =
    [ Option ['b'] ["box"] 
                 (ReqArg (\b opts -> opts {box = b}) "path/to/box")
                 ("Path to the box. Defaults to " ++ defaultBox ++ ".")
    , Option ['d'] ["document"]
                 (ReqArg (\d opts -> opts {doc = d}) "path/to/doc")
                 "Path to the document to add"
    , Option ['T'] ["title"]
                 (ReqArg (\t opts -> opts {title = Just t}) "doc title")
                 "Title of the document"
    , Option ['a'] ["author"]
                 (ReqArg (\as opts -> opts {authors = Just (splinter ';' as)})
                             "L1,F1,M1;L2,F2,M2")
                 "Authors' names separated by semicolons"
    , Option ['t'] ["tags"]
                 (ReqArg (\ts opts -> opts {tags = Just (splinter ',' ts)}) "foo,bar,baz")
                 "Comma separated tags for the document"
    ]


splinter :: Char -> String -> [String]
splinter c = reverse .foldl (\acc c' -> if c == c' then [] : acc else (head acc ++ [c']) : (tail acc)) [[]]



parseArgs :: [String] -> IO (Options, [String])
parseArgs argv =
    case getOpt Permute options argv of
      (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
        where header = "Usage: " ++ me ++ " OPTIONS"


genArgs Nothing     = pure Nothing
genArgs (Just opts) = do
  let b = box opts
      f = doc opts
  h <- Doc.doHash f
  let d = Doc.Document {
            Doc.title = genTitle opts
          , Doc.authors  = genAuthors opts
          , Doc.tags     = genTags opts
          , Doc.hash     = h 
          , Doc.context   = Doc.Context { Doc.suffix = takeExtension f
                                        , Doc.application = Doc.DummyApplication }
          }
  pure . pure $ (f,d,b)

genTitle = Doc.Title . fromJust . title
genAuthors = S.fromList . map mkAuthor . fromJust . authors
mkAuthor s = Doc.Author {
               Doc.lastname   = (s' !! 0)
             , Doc.firstname  = let s'' = (s' !! 1) in if null s'' then Nothing else Just s''
             , Doc.middlename = let s'' = (s' !! 2) in if null s'' then Nothing else Just s''
             }
    where s' = splinter ',' s ++ ["",""]
genTags = S.fromList . map Doc.Tag . fromJust . tags


hash :: FilePath -> IO Digest
hash = fmap (fmap sha1) BS.readFile

clean :: Options -> IO (Maybe Options)
clean opts = do
  cleanedBox <- canonicalizePath $ box opts
  cleanedDoc <- canonicalizePath $ doc opts
  pure . Just $ opts { box = cleanedBox
                     , doc = cleanedDoc }

main =
    getArgs      >>=
    parseArgs    >>=
    pure . fst   >>=
    clean        >>=
    genArgs      >>=
    run
        where run Nothing   = pure ()
              run (Just (f,d,b)) = Doc.addDocument f b d
