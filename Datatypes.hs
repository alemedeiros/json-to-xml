-- Datatypes.hs
--  by Alexandre Medeiros <alexandre.n.medeiros _at_ gmail.com>
--     Tom Hedges <t.w.hedges _at_ qmul.ac.uk>
--
-- JSON to XML translation using Aeson
--
-- Datatypes definition and classtype implementations

{-# LANGUAGE OverloadedStrings #-}

module Datatypes where

import Control.Applicative
import Control.Monad

import Data.Aeson
import Data.Functor (fmap)
import Data.Maybe

import Text.Read (readMaybe)

--import Debug.Trace

{- Datatypes definition; matching json fields -}

data Artist = NullArtist | Artist
        { artistAliases :: [Alias]
        , artistArea :: Area
        , artistBeginArea :: Area
        , artistCountry :: String
        , artistDisambig :: String
        , artistEndArea :: Area
        , artistID :: String
        , artistISNI :: [String]
        , artistLifeSpan :: LifeSpan
        , artistName :: String
        , artistRating :: Rating
        , artistSortName :: String
        , artistTags :: [Tag]
        , artistType :: String
        } deriving (Show, Eq)

data Alias = NullAlias | Alias
        { aliasLocale :: String
        , aliasName :: String
        , aliasPrimary :: Bool
        , aliasSortName :: String
        , aliasType :: String
        } deriving (Show, Eq)

data Area = NullArea | Area
        { areaDisambig :: String
        , areaID :: String
        , areaISO1 :: [String]
        , areaISO2 :: [String]
        , areaISO3 :: [String]
        , areaName :: String
        , areaSortName :: String
        } deriving (Show, Eq)

data LifeSpan = NullLifeSpan | LifeSpan
        { lifeSpanBegin :: String -- Change to a date type or something like YYYY-MM-DD ?
        , lifeSpanEnd :: String
        , lifeSpanEnded :: Bool
        } deriving (Show, Eq)

data Rating = EmptyRating | Rating
        { ratingValue :: Double
        , ratingCount :: Int
        } deriving (Show, Eq)

data Tag = NullTag | Tag
        { tagCount :: Int
        , tagName :: String
        } deriving (Show, Eq, Ord)

{- Implementation of Ord typeclass for Rating and Artist Datatypes -}
instance Ord Rating where
        compare (Rating vA cA) (Rating vB cB) = compare vA vB
        compare   (Rating _ _)              _ = GT
        compare              _   (Rating _ _) = LT
        compare              _              _ = EQ

instance Ord Artist where
        compare Artist{artistRating=rA} Artist{artistRating=rB} = compare rA rB
        compare                Artist{}                       _ = GT
        compare                       _                Artist{} = LT
        compare                       _                       _ = EQ

{-
- Implementation of FromJSON typeclass for all the datatypes.
- Basicaly associates each json field with a field on the datatypes.
-}
instance FromJSON Artist where
        --parseJSON (Object v) | trace ("=> Artist: " ++ show v) False = undefined
        parseJSON (Object v) = Artist <$>
                v .:? "alias-list"      .!= [] <*>
                v .:? "area"            .!= defaultArea <*>
                v .:? "begin-area"      .!= defaultArea <*>
                v .:? "country"         .!= "" <*>
                v .:? "disambiguation"  .!= "" <*>
                v .:? "end-area"        .!= defaultArea <*>
                v .:? "id"              .!= "" <*>
                v .:? "isni-list"       .!= [] <*>
                v .:? "life-span"       .!= defaultLifeSpan <*>
                v .:? "name"            .!= ""<*>
                v .:? "rating"          .!= defaultRating <*>
                v .:? "sort-name"       .!= "" <*>
                v .:? "tag-list"        .!= [] <*>
                v .:? "type"            .!= ""
        parseJSON _ = fail "fail at Artist"

instance FromJSON Alias where
        --parseJSON (Object v) | trace ("=> Alias: " ++ show v) False = undefined
        parseJSON (Object v) = Alias <$>
                v .:? "locale"    .!= "" <*>
                v .:? "alias"     .!= "" <*>
                fmap isPrim (v .:? "primary" .!= "") <*>
                v .:? "sort-name" .!= "" <*>
                v .:? "type"      .!= ""
                        where
                                isPrim :: String -> Bool
                                isPrim = (=="primary")
        parseJSON _ = fail "fail at Alias"

instance FromJSON Area where
        --parseJSON (Object v) | trace ("=> Area: " ++ show v) False = undefined
        parseJSON (Object v) = Area <$>
                v .:? "disambiguation"        .!= "" <*>
                v .:? "id"                    .!= "" <*>
                v .:? "iso-3166-1-code-list"  .!= [] <*>
                v .:? "iso-3166-2-code-list"  .!= [] <*>
                v .:? "iso-3166-3-code-list"  .!= [] <*>
                v .:? "name"                  .!= "" <*>
                v .:? "sort-name"             .!= ""
        parseJSON _ = fail "fail at Area"

instance FromJSON LifeSpan where
        --parseJSON (Object v) | trace ("=> LifeSpan: " ++ show v) False = undefined
        parseJSON (Object v) = LifeSpan <$>
                v .:? "begin" .!= "" <*>
                v .:? "end"   .!= "" <*>
                fmap isTrue (v .:? "ended" .!= "")
                        where
                                isTrue :: String -> Bool
                                isTrue = (=="true")
        parseJSON _ = fail "fail at LifeSpan"

instance FromJSON Rating where
        --parseJSON (Object v) | trace ("=> Rating: " ++ show v) False = undefined
        parseJSON (Object v) = Rating <$>
                fmap readDouble (v .:? "rating"       .!= "") <*>
                fmap readInt    (v .:? "votes-count"  .!= "")
        parseJSON _ = fail "fail at Rating"

instance FromJSON Tag where
        --parseJSON (Object v) | trace ("=> Tag: " ++ show v) False = undefined
        parseJSON (Object v) = Tag <$>
                fmap readInt  (v .:? "count" .!= "") <*>
                v .:? "name"  .!= ""
        parseJSON _ = fail "fail at Tag"

{- Field parsing functions. -}
readInt    = fromMaybe (-1) . readMaybe
readDouble = fromMaybe  0.0 . readMaybe

defaultAlias = Alias { aliasLocale = ""
                     , aliasName = ""
                     , aliasPrimary = False
                     , aliasSortName = ""
                     , aliasType = ""
                     }

defaultArea = Area {areaDisambig = ""
                   , areaID = ""
                   , areaISO1 = []
                   , areaISO2 =  []
                   , areaISO3 = []
                   , areaName = ""
                   , areaSortName = "" }

defaultLifeSpan = LifeSpan {lifeSpanBegin = ""
                  , lifeSpanEnd = ""
                  , lifeSpanEnded = False
                  }

defaultRating = Rating {ratingValue = 0.0
                       , ratingCount = 0
                       } 
defaultTag = Tag { tagCount = 0
                 , tagName = ""
                   }


