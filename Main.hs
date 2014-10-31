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
        putStrLn "Query 0: Primary artists alias"
        print . maybeClean $ map getPrimAlias artistData
        putStrLn "Query 1: Tags - Sorted by greater count first"
        print . maybeClean $ map getTags artistData
        putStrLn "Query 2: Artists that are from a specific country"
        print . map artistName . filter (isFrom "GB") $ artistData
        putStrLn "Query 3: End date"
        print . maybeClean $ map getEndDate artistData

maybeClean :: Eq a => [Maybe a] -> [a]
maybeClean = map fromJust . filter (/= Nothing)

-- Query 0:
-- Return artist primary alias if there is one or Nothing otherwise
getPrimAlias :: Artist -> Maybe (String,String)
getPrimAlias (Artist aliases _ _ _ _ _ _ _ _ n _ _ _ _) = primAlias aliases
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
getTags (Artist _ _ _ _ _ _ _ _ _ _ _ _ [] _) = Nothing
getTags (Artist _ _ _ _ _ _ _ _ _ n _ _ tags _) = Just (n, sortTags)
        where
                sortTags = sortBy (flip compare) tags
getTags _ = Nothing

-- Query 2:
-- Returns whether an artist is from the country named on String
isFrom :: String -> Artist -> Bool
isFrom str (Artist _ _ _ country _ _ _ _ _ _ _ _ _ _) = str == country
isFrom _ _ = False

-- Query 3:
-- If the Artist ended its career, returns its ending date, otherwise, returns
-- nothing
getEndDate :: Artist -> Maybe (String, String)
getEndDate (Artist _ _ _ _ _ _ _ _ (LifeSpan _ endDate ended) n _ _ _ _)
        | ended  = Just (n, endDate)
getEndDate _ = Nothing
