module Main where

import Data.Monoid
import Data.Maybe
import System.FilePath
import System.Directory
import Control.Monad
import qualified Data.Map as Map

data FileType = Document
              | Picture
              | Video
              | Unknown
  deriving (Show, Eq, Ord)

type FileMap = Map.Map FileType [FilePath]

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

classifyFilesMap :: [FilePath] -> FileMap
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

classifyAllFilesInDirectory :: FilePath -> [FilePath] -> IO (FileMap)
classifyAllFilesInDirectory baseDir excludedDirs = do
  allFiles <- getAllFiles baseDir excludedDirs
  let fileTypeMap = classifyFilesMap allFiles
  return fileTypeMap

filterFileType :: FileType -> FileMap -> Maybe [FilePath]
filterFileType fileType fileMap = Map.lookup fileType fileMap

-- basically same as filterFileType, but nice printing
prettyFileMap :: FilePath -> [FilePath] -> IO ()
prettyFileMap baseDir excludedDirs = do
  fileTypeMap <- classifyAllFilesInDirectory baseDir excludedDirs

  mapM_ (\(fileType, files) -> do
            putStrLn $ "FileType: " ++ show fileType
            mapM_ putStrLn files
        ) (Map.toList fileTypeMap)

main :: IO ()
main = do
  let baseDir = "/home/bard/Pictures/wallpaper"
  let excludedDirs = [".git" , "node_modules"]
  -- fileTypeMap <- classifyAllFilesInDirectory baseDir excludedDirs
  -- putStrLn $ show $ filterFileType Picture fileTypeMap

  prettyFileMap baseDir excludedDirs
