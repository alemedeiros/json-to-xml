-- Main.hs
--  by Alexandre Medeiros <alexandre.n.medeiros _at_ gmail.com>
--     Tom Hedges <t.w.hedges _at_ qmul.ac.uk>
--
-- JSON to XML translation using Aeson
--
-- Main file for ECS713 Functional Programming Project

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Datatypes
import Queries
import ToXml

import Control.Monad

import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Data.Maybe

import System.Directory
import System.Environment
import System.FilePath
import System.IO

import Text.XML.Light

-- Main function
main :: IO ()
main = do
        -- Get file names from arguments
        fileNames <- getArgs
        artistData <- runFromFileList fileNames

        -- Call example queries
        doQueries artistData

{--- Parsing and conversion functions ---}

-- Run parsing and conversion on a list of files, saves the converted xml into
-- file and returns the internal artist representation for queries
runFromFileList :: [FilePath] -> IO [Artist]
runFromFileList filePaths = do
        -- Get file content
        dataStr <- mapM BS.readFile filePaths

        -- Convert data
        let
            -- json to internal representation
            artistData = map readArtist dataStr
            -- internal representation to xml module representation
            xmlData = map makeXmlArtist artistData
            -- xml module representation to xml string representation
            xmlStr = map showElement xmlData

        {- DEBUG
        -- Print internal datatype
        putStrLn " ========== Showing data ========== "
        print artistData
        -}

        {- DEBUG
        -- Print xml
        putStrLn " ========== Showing XML ========== "
        print xmlStr
        -}

        -- Write xml data to output files
        zipWithM_ writeToFile xmlData (map generateFilePath filePaths)

        return artistData


-- Run the json-to-XML on a directory of json files.
runFromDirectory :: FilePath -> IO [Artist]
runFromDirectory dir = do
        -- Get json files on directory
        fileNames <- getDirectoryContents dir
        let filteredFileNames = filterJsonFile fileNames
            fileDirs = replicate (length filteredFileNames) dir
            filePaths = zipWith combine fileDirs filteredFileNames

        -- Run on all files on directory
        runFromFileList filePaths

{--- Auxiliar functions ---}

-- Parse a json string into an Artist
readArtist :: BS.ByteString -> Artist
readArtist = fromMaybe NullArtist . decode

-- Write internal xml representation to file
writeToFile :: Element -> FilePath -> IO ()
writeToFile xml fileName = writeFile fileName $ showElement xml

-- Create the filepath for output xml file
generateFilePath :: String -> FilePath
generateFilePath jsonFileName =
        replaceDirectory (replaceExtension jsonFileName "xml") "./xml/"

-- Filter files
filterJsonFile :: [String] -> [String]
filterJsonFile = filter (\(x:_) -> x /= '.')
