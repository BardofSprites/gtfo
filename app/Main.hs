module Main where

import Data.Monoid
import Data.Maybe
import System.FilePath
import System.Posix.Files
import System.Directory
import Control.Monad
import Control.Exception (catch, SomeException)
import qualified Data.Map as Map

data FileType = Document
              | Picture
              | Video
              | Archive
              | Unknown
  deriving (Show, Eq, Ord, Enum, Bounded)

type FileMap = Map.Map FileType [FilePath]

documentExt :: [String]
documentExt = [".pdf", ".docx", ".txt", ".org"]

pictureExt :: [String]
pictureExt = [".jpg", ".jpeg", ".xbm", ".bmp", ".png", ".gif"]

videoExt :: [String]
videoExt = [".mp4", ".avi", ".mov"]

archiveExt :: [String]
archiveExt = [".zip", ".tar.gz"]

-- assigns type based on extension
getFileType :: FilePath -> FileType
getFileType path
    | ext `elem` documentExt = Document
    | ext `elem` pictureExt  = Picture
    | ext `elem` videoExt    = Video
    | otherwise = Unknown
  where
    ext = takeExtension path

-- getAllFiles from a dir, excluding dirs, return list of files
getAllFiles :: FilePath -> [FilePath] -> IO [FilePath]
getAllFiles baseDir excludedDirs = do
  entries <- listDirectory baseDir
  let fullEntries = map (baseDir </>) entries
  files <- filterM doesFileExist fullEntries
  dirs <- filterM doesDirectoryExist fullEntries
  let dirsToSearch = filter (\dir -> takeFileName dir `notElem` excludedDirs) dirs
  nestedFiles <- forM dirsToSearch $ \dir -> getAllFiles dir excludedDirs
  return (files ++ concat nestedFiles)

-- classifyFilesMap: classify files into a map based on their type
-- [(Document, [All the documents]), (Picture, [all the picture files]), etc...]
classifyFilesMap :: [FilePath] -> FileMap
classifyFilesMap = foldr insertFileType Map.empty
  where
    insertFileType path acc =
        let fileType = getFileType path
        in Map.insertWith (++) fileType [path] acc

-- fileTypeFilter: filter files by file type from a dir, excluding dirs
fileTypeFilter :: FilePath -> [FilePath] -> FileType -> IO [FilePath]
fileTypeFilter dir excludedDirs fileType = do
    files <- getAllFiles dir excludedDirs
    return $ fromMaybe [] (Map.lookup fileType (classifyFilesMap files))

copyFiles :: FileMap -> FilePath -> IO ()
copyFiles fileMap destination = do
  forM_ (Map.toList fileMap) $ \(fileType, files) -> do
    let destDir = destination </> show fileType
    createDirectoryIfMissing True destDir
    forM_ files $ \file -> do
      let destFile = destDir </> takeFileName file
      copyFile file destFile
      putStrLn $ "Copied " ++ file ++ " to " ++ destFile

main :: IO()
main = do
  let baseDir = "/home/bard/testo-besto"
  let excludedDirs = [".git", "node_modules"]
  fileMap <- classifyFilesMap <$> getAllFiles baseDir excludedDirs
  let destinationDir = "/home/bard/testo-even-bestoer"
  copyFiles fileMap destinationDir
