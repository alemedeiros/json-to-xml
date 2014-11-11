-- Queries.hs
--  by Alexandre Medeiros <alexandre.n.medeiros _at_ gmail.com>
--     Tom Hedges <t.w.hedges _at_ qmul.ac.uk>
--
-- JSON to XML translation using Aeson
--
-- Queries to be made on the internal data representation

module Queries where

import Datatypes

import Data.List
import Data.Maybe

{- Sample queries -}
doQueries :: [Artist] -> IO ()
doQueries artistData = do
        -- Queries examples should be introduced here
        putStrLn "Query 0: Primary artists alias"
        print . maybeClean $ map getPrimAlias artistData
        putStrLn "Query 1: Tags - Sorted by greater count first"
        print . maybeClean $ map getTags artistData
        putStrLn "Query 2: Artists that are from a specific country (US)"
        print . map artistName . filter (isFrom "US") $ artistData
        putStrLn "Query 3: End date"
        print . maybeClean $ map getEndDate artistData
                where
                        maybeClean :: Eq a => [Maybe a] -> [a]
                        maybeClean = map fromJust . filter (/= Nothing)

{-
- Query 0:
- Return artist primary alias if there is one or Nothing otherwise
-}
getPrimAlias :: Artist -> Maybe (String,String)
getPrimAlias Artist{artistAliases = aliases, artistName = n} = primAlias aliases
        where
                primAlias :: [Alias] ->  Maybe (String, String)
                primAlias [] = Nothing
                primAlias (Alias _ alias prim _ _:xs)
                        | prim      = Just (n, alias)
                        | otherwise = primAlias xs
                primAlias _ = Nothing
getArtistPrimAlias _ = Nothing

{-
- Query 1:
- Return Tags list sorted by Tag count
-}
getTags :: Artist -> Maybe (String, [Tag])
getTags Artist{ artistTags = []                   } = Nothing
getTags Artist{ artistTags = tags, artistName = n } = Just (n, sortTags)
        where
                sortTags = sortBy (flip compare) tags
getTags _ = Nothing

{-
- Query 2:
- Returns whether an artist is from the country named on String
-
- Some of the known values are: "US", "GB", "BR", "AU" and "JP".
-}
isFrom :: String -> Artist -> Bool
isFrom str Artist{artistCountry = country} = str == country
isFrom   _                               _ = False

{-
- Query 3:
- If the Artist ended its career, returns its ending date, otherwise, returns
- nothing
-}
getEndDate :: Artist -> Maybe (String, String)
getEndDate Artist{ artistLifeSpan = (LifeSpan _ endDate ended), artistName = n}
        | ended  = Just (n, endDate)
getEndDate _ = Nothing
