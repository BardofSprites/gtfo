module Main where

import Data.Monoid
import System.FilePath
import System.Directory
import Control.Monad
import qualified Data.Map as Map

data FileType = Document
              | Picture
              | Video
              | Unknown
  deriving (Show, Eq, Ord)

instance Monoid FileType where
  mempty = Unknown
  mappend Unknown x = x
  mappend x Unknown = x
  mappend x _ = x

instance Semigroup FileType where
  (<>) = mappend

documentExt :: [String]
documentExt = [".pdf", ".docx", ".txt", ".org"]

pictureExt :: [String]
pictureExt = [".jpg", ".jpeg", ".xbm", ".bmp", ".png", ".gif"]

videoExt :: [String]
videoExt = [".mp4", ".avi", ".mov"]

getFileType :: FilePath -> FileType
getFileType path
    | ext `elem` documentExt = Document
    | ext `elem` pictureExt  = Picture
    | ext `elem` videoExt    = Video
    | otherwise = Unknown
  where
    ext = takeExtension path

filterByType :: FileType -> [FilePath] -> [FilePath]
filterByType fileType allFiles = filter (\path -> getFileType path == fileType) allFiles

mapFileType :: [FilePath] -> IO ()
mapFileType files = mapM_ print (classifyFilesMap files)

classifyFilesMap :: [FilePath] -> Map.Map FileType [FilePath]
classifyFilesMap = foldr insertFileType Map.empty
  where
    insertFileType path acc =
        let fileType = getFileType path
        in Map.insertWith (++) fileType [path] acc

getAllFiles :: FilePath -> [FilePath] -> IO [FilePath]
getAllFiles baseDir excludedDirs = do
  entries <- listDirectory baseDir

  let fullEntries = map (baseDir </>) entries
  files <- filterM doesFileExist fullEntries
  dirs <- filterM doesDirectoryExist fullEntries

  let dirsToSearch = filter (\dir -> takeFileName dir `notElem` excludedDirs) dirs
  nestedFiles <- forM dirsToSearch $ \dir -> getAllFiles dir excludedDirs
  return (files ++ concat nestedFiles)

classifyAllFilesInDirectory :: FilePath -> [FilePath] -> IO (Map.Map FileType [FilePath])
classifyAllFilesInDirectory baseDir excludedDirs = do
  allFiles <- getAllFiles baseDir excludedDirs
  let fileTypeMap = classifyFilesMap allFiles
  return fileTypeMap


testFunc :: IO ()
testFunc = do
  let baseDir = "/home/bard/Pictures/wallpaper"
  let excludedDirs = [".git", "node_modules"]
  fileTypeMap <- classifyAllFilesInDirectory baseDir excludedDirs

  mapM_ (\(fileType, files) -> do
            putStrLn $ "FileType: " ++ show fileType
            mapM_ putStrLn files
        ) (Map.toList fileTypeMap)

main :: IO ()
main = do
  putStrLn "Hello world"

  -- let baseDir = "/home/bard/Notes/denote"
  -- let excludedDirs = [".git", "ltximg"]
  -- allFiles <- getAllFiles baseDir excludedDirs
  -- mapFileType allFiles
