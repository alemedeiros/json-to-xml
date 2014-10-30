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
import Data.ByteString.Lazy
import Data.Maybe

import System.Environment

{-
main :: IO ()
main = print $ fromJust aesonSample
-}

readArtist :: ByteString -> Maybe Artist
readArtist = decode

main :: IO ()
main = do
        fn:_ <- getArgs
        contents <- Data.ByteString.Lazy.readFile fn
        print $ readArtist contents
