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
