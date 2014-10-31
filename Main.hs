-- Main.hs
--  by alemedeiros <alexandre.n.medeiros _at_ gmail.com>
--
-- JSON to XML translation using Aeson
--
-- Main file for ECS713 Functional Programming Project

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Datatypes

import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Data.List
import Data.Maybe

import System.Environment

-- This functions gets the string contents of a json file and returns an artist
readArtist :: BS.ByteString -> Artist
readArtist = fromMaybe NullArtist . decode

main :: IO ()
main = do
        args <- getArgs
        dataStr <- mapM BS.readFile args
        let
            artistData = map readArtist dataStr
        putStrLn "Showing read data"
        print artistData
        -- Queries examples should be introduced here
        putStrLn "Query 0: Primary artists name"
        print . filter (/= Nothing) $ map getArtistPrimAlias artistData
        putStrLn "Query 1: Tags - Sorted by greater count first"
        print . filter (/= Nothing) $ map getTags artistData
        putStrLn "Query 2: Artists that are from a specific country"
        print . filter (isFrom "GB") $ artistData

-- Query 0:
-- Return artist primary alias if there is one or Nothing otherwise
getArtistPrimAlias :: Artist -> Maybe String
getArtistPrimAlias (Artist aliases _ _ _ _ _ _ _ _ _ _ _ _ _) = primAlias aliases
        where
                primAlias :: [Alias] -> Maybe String
                primAlias [] = Nothing
                primAlias ((Alias _ alias prim _ _):xs)
                        | prim      = Just alias
                        | otherwise = primAlias xs
                primAlias _ = Nothing
getArtistPrimAlias _ = Nothing

-- Query 1:
-- Return Tags list sorted by Tag count
getTags :: Artist -> Maybe [Tag]
getTags (Artist _ _ _ _ _ _ _ _ _ _ _ _ [] _) = Nothing
getTags (Artist _ _ _ _ _ _ _ _ _ _ _ _ tags _) = Just sortTags
        where
                sortTags = sortBy (flip compare) tags
getTags _ = Nothing

-- Query 2:
-- Returns whether an artist is from the country named on String
isFrom :: String -> Artist -> Bool
isFrom str (Artist _ _ _ country _ _ _ _ _ _ _ _ _ _) = str == country
isFrom _ _ = False
