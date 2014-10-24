-- Main.hs
--  by alemedeiros <alexandre.n.medeiros _at_ gmail.com>
--
-- JSON to XML translation using Aeson
--
-- Main file for ECS713 Functional Programming Project

module Main where

import AesonSample

import Data.Maybe

main :: IO ()
main = print $ fromJust aesonSample
