-- Main.hs
--  by alemedeiros <alexandre.n.medeiros _at_ gmail.com>
--
-- JSON to XML translation using Aeson
--
-- Main file for ECS713 Functional Programming Project

{-# LANGUAGE OverloadedStrings #-}

module Main where

--import AesonSample
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
        print $ map readArtist dataStr
