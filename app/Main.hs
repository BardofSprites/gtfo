module Main where

import System.Directory (listDirectory)
import Data.Text (isSuffixOf)
import Data.List (filter)
-- import System.FilePath ()

main :: IO ()
main = putStrLn("Hello world")

getFiles :: FilePath -> IO [FilePath]
getFiles dir = do
    files <- listDirectory dir
    return files

filterByExtension :: String -> [FilePath] -> [FilePath]
filterByExtension ext files = filter (isSuffixOf ext) files

