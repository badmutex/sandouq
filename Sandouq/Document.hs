module Sandouq.Document where

import Control.Monad
import Control.Monad.Instances
import Data.Digest.Pure.SHA
import Data.List
import Data.Maybe (isJust, fromJust)
import System.FilePath
import System.Directory
import System.Posix.Files
import qualified Data.Set as S
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSI

import Sandouq.Config


findBlob b (Hash h) = b </> hiddenRoot </> show Blobs </> h

{- ******************** Data Structures ******************** -}


data Document = Document {
      title   :: Title
    , authors :: S.Set Author
    , tags    :: S.Set Tag
    , context :: Context
    , hash    :: Hash
    } deriving (Read, Show)

fromFileName d = (fromTitle . title) d <.> (suffix . context) d

prettyDocument d = let t  ="Title:\t\t " ++ (fromTitle . title) d ++ "\n"
                       as = list "Authors:\t " fromAuthor authors "\t\t\t"
                       ts = list "Tags:\t\t " fromTag tags "\t\t\t"
                       list c f f' ts = let as = intersperse ("\n" ++ ts) .
                                                 map f . S.toList . f' $ d
                                        in c ++ head as ++
                                               (concat . tail) as
                                               ++ "\n"
                       h = "Hash:\t\t " ++ (fromHash . hash) d
                   in t ++ as ++ ts ++ h


newtype Hash = Hash String deriving (Read, Show)
fromHash (Hash s) = s

newtype Title = Title String deriving (Read, Show)
fromTitle (Title t) = t

data Author = Author {
      lastname   :: String
    , firstname  :: Maybe String
    , middlename :: Maybe String
    } deriving (Eq, Ord, Read, Show)
fromAuthor a = lastname a ++ fname a ++ mname a
    where fname    = name firstname ", "
          mname    = name middlename ". "
          name f p a = if isJust . f $ a then ((++) p . fromJust . f) a else ""

newtype Tag = Tag String deriving (Eq, Ord, Read, Show)
fromTag (Tag t) = t

data Context = Context {
      suffix :: String
    , application :: Application
    } deriving (Read, Show)



data Status = FileCreated FilePath 
            | FileExists FilePath
              deriving Show


-- | Dummy. For future extension.
data Application = DummyApplication
                 deriving (Read, Show)



{- ******************** Functions ******************** -}

authorOp f a d = d {authors = f a $ authors d}
tagOp    f t d = d {tags    = f t $ tags d}

addAuthor, rmAuthor :: Author -> Document -> Document

-- | Add an author to a document
addAuthor = authorOp S.insert
-- | Remove and author from a document
rmAuthor  = authorOp S.delete

addTag, rmTag :: Tag -> Document -> Document

-- | Add a tag to a document
addTag = tagOp S.insert
-- | Remove a tag from a document
rmTag  = tagOp S.delete








-- | Links a document to an author in the box
linkToAuthor :: FilePath -> Author -> Document -> IO ()
linkToAuthor = targetLink (\a -> show Authors </> fromAuthor a)

-- | Links a document to a tag in the box
linkToTag :: FilePath -> Tag -> Document -> IO ()
linkToTag = targetLink (\a -> show Tags </> fromTag a)

targetLink :: Show a =>
              (a -> String) -- ^ 'fromTags' or 'fromAuthors'
           -> FilePath      -- ^ path to the root of the box
           -> a             -- ^ 'Tags' or 'Authors'
           -> Document
           -> IO ()
targetLink getter root a doc = let target = "."   </>
                                            getter a </>
                                            fromFileName doc
                                   blob   = findBlob root . hash $ doc
                               in link blob target




-- | Makes a relative symbolic link from a blob to the target file.
link :: FilePath -- ^ Origin
     -> FilePath -- ^ Target
     -> IO ()
link o t = let (o',td,t') = (mkRelative o t, takeDirectory t, takeFileName t)
           in do cwd <- getCurrentDirectory
                 createDirectoryIfMissing True td
                 setCurrentDirectory td
                 createSymbolicLink o' t'
                 setCurrentDirectory cwd


-- | Make the first parameter relative to the second within the box.
--
-- @ mkRelative ".sandouq/Blobs/16657d2796145c37f33b865b833bbe85a3661bf3" "Tags/foo/bar.ext" @
-- >   "../../.sandouq/Blobs/16657d2796145c37f33b865b833bbe85a3661bf3"
mkRelative :: FilePath -> FilePath -> FilePath
mkRelative o =  foldl1 (</>)                   .
                flip (++) (dropWhile (\s -> s /= hiddenRoot) $ splitDirectories o) .
                map (\_ -> "..")               .
                dropLast                       .
                splitDirectories
                       where dropLast xs = take (length xs - 2) xs



-- | Links the authors and tags for a document
linkDocument :: FilePath -> Document -> IO ()
linkDocument b d = let as        = work linkToAuthor authors
                       ts        = work linkToTag    tags
                       work lf f = mapM_ (flip (lf b) d) (S.toList . f $ d)
                    in do as; ts


-- | Writes the Document to it's hash in .sandouq/Docs
writeDocument :: FilePath -> Document -> IO ()
writeDocument b d = let t = b          </>
                            hiddenRoot </>
                            show Docs  </>
                            (fromHash . hash) d
                        d' = BS.pack . show $ d
                    in do createDirectoryIfMissing False (takeDirectory t)
                          fe <- doesFileExist t
                          case fe of
                            False -> BS.writeFile t d'
                            _     -> error $ "Cannot overwrite " ++ (fromTitle . title) d ++ " to " ++ t




doHash :: FilePath -> IO Hash
doHash f = fmap (Hash . show) $ fmap sha1 (BSI.readFile f)





addDocument :: FilePath -> FilePath -> Document -> IO ()
addDocument f b d = let h = findBlob b (hash d)
                    in do cwd <- getCurrentDirectory
                          setCurrentDirectory b

                          copyFile f h
                          writeDocument b d
                          linkDocument b d

                          setCurrentDirectory cwd

test = let a = S.fromList [
                 Author { lastname = "Abdul-Wahid"
                       , firstname = Just "C"
                       , middlename = Just "Badi'" }
               , Author { lastname = "baz"
                        , firstname = Just "foo"
                        , middlename = Nothing}
               ]
           ts = S.fromList [Tag "foo", Tag "bar", Tag "baz"]
           t = Title "Badi's Resume"
           c = Context { suffix = "pdf", application = DummyApplication }
           h = Hash "1422f241f65ca32a5135af667c7a639ce1439572"
           d = Document {
                 title = t
               , authors = a
               , tags = ts
               , context = c
               , hash = h }
       in d
