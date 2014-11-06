-- Main.hs
--  by alemedeiros <alexandre.n.medeiros _at_ gmail.com>
--
-- JSON to XML translation using Aeson
--
-- Main file for ECS713 Functional Programming Project

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Datatypes
import ToXml
import System.FilePath
import Text.XML.Light

import Control.Monad

import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Data.List
import Data.Maybe

import System.IO

import System.Environment
import System.Directory

-- This functions gets the string contents of a json file and returns an artist
readArtist :: BS.ByteString -> Artist
readArtist = fromMaybe NullArtist . decode

main :: IO ()
main = do
        fileNames <- getArgs
        dataStr <- mapM BS.readFile fileNames
        let
            artistData = map readArtist dataStr
        putStrLn "Showing read data"
        print artistData

       --convert to xml and put in file
        let xmlData = map makeXmlArtist artistData
        putStrLn "Showing XML"
        print xmlData
        zipWithM writeToFile xmlData (map generateFilePath fileNames)
        
        doQueries artistData

        putStrLn "End"

-- This works, main doesn't. No idea why
-- error is json-to-xml: No match in record selector artistAliases
-- after line putStrLn xmlData
        
runFromFileList :: [FilePath] -> IO ()
runFromFileList filePaths = do

        dataStr <- mapM BS.readFile filePaths
        let
            artistData = map readArtist dataStr
        putStrLn "Showing read data"
        print artistData

       --convert to xml and put in file
        let xmlData = map makeXmlArtist artistData
        putStrLn "Showing XML"
        print xmlData
        zipWithM writeToFile xmlData (map generateFilePath filePaths)

        doQueries artistData
        
        putStrLn "End"

-- Runs the json-to-XML from a directory of json files.
-- This works, only if there aren't too many files in the dir.
runFromDirectory :: FilePath -> IO ()
runFromDirectory dir = do

  fileNames <- getDirectoryContents dir
  let filteredFileNames = filterJsonFile fileNames
  let fileDirs = replicate (length filteredFileNames) dir
  let filePaths = zipWith combine fileDirs filteredFileNames
  runFromFileList filePaths
        
doQueries :: [Artist] -> IO ()
doQueries artistData = do
   -- Queries examples should be introduced here
        putStrLn "Query 0: Primary artists alias"
        print . maybeClean $ map getPrimAlias artistData
        putStrLn "Query 1: Tags - Sorted by greater count first"
        print . maybeClean $ map getTags artistData
        putStrLn "Query 2: Artists that are from a specific country"
        print . map artistName . filter (isFrom "US") $ artistData
        putStrLn "Query 3: End date"
        print . maybeClean $ map getEndDate artistData

maybeClean :: Eq a => [Maybe a] -> [a]
maybeClean = map fromJust . filter (/= Nothing)

-- Query 0:
-- Return artist primary alias if there is one or Nothing otherwise
getPrimAlias :: Artist -> Maybe (String,String)
getPrimAlias Artist{artistAliases = aliases, artistName = n} = primAlias aliases
        where
                primAlias :: [Alias] ->  Maybe (String, String)
                primAlias [] = Nothing
                primAlias ((Alias _ alias prim _ _):xs)
                        | prim      = Just (n, alias)
                        | otherwise = primAlias xs
                primAlias _ = Nothing
getArtistPrimAlias _ = Nothing

-- Query 1:
-- Return Tags list sorted by Tag count
getTags :: Artist -> Maybe (String, [Tag])
getTags Artist{ artistTags = []                   } = Nothing
getTags Artist{ artistTags = tags, artistName = n } = Just (n, sortTags)
        where
                sortTags = sortBy (flip compare) tags
getTags _ = Nothing

-- Query 2:
-- Returns whether an artist is from the country named on String
isFrom :: String -> Artist -> Bool
isFrom str Artist{artistCountry = country} = str == country
isFrom   _                               _ = False

-- Query 3:
-- If the Artist ended its career, returns its ending date, otherwise, returns
-- nothing
getEndDate :: Artist -> Maybe (String, String)
getEndDate Artist{ artistLifeSpan = (LifeSpan _ endDate ended), artistName = n}
        | ended  = Just (n, endDate)
getEndDate _ = Nothing

-- Functions for writing xmls to file

writeToFile :: Element -> FilePath -> IO ()
writeToFile xml fileName = 
  writeFile fileName $ showElement xml

generateFilePath :: String -> FilePath
generateFilePath jsonFileName =
  replaceDirectory (replaceExtension jsonFileName "xml") "./xmls/"

filterJsonFile :: [String] -> [String]
filterJsonFile files = filter notFile files
  where notFile x = not (head x == '.')
