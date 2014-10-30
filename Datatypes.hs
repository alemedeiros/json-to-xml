-- Datatypes.hs
--  by alemedeiros <alexandre.n.medeiros _at_ gmail.com>

{-# LANGUAGE OverloadedStrings #-}

module Datatypes where

import Control.Applicative
import Control.Monad

import Data.Aeson
import Data.Functor (fmap)
import Data.Maybe

import Text.Read (readMaybe)

--import Debug.Trace

data Artist = Artist
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
        } deriving (Show)

data Alias = Alias
        { aliasLocale :: String
        , aliasName :: String
        , aliasPrimary :: Bool
        , aliasSortName :: String
        , aliasType :: String
        } deriving (Show)

data Area = NullArea | Area
        { areaDisambig :: String
        , areaID :: String
        , areaISO1 :: [String]
        , areaISO2 :: [String]
        , areaISO3 :: [String]
        , areaName :: String
        , areaSortName :: String
        } deriving (Show)

data LifeSpan = NullLifeSpan | LifeSpan
        { begin :: String -- Change to a date type or something like YYYY-MM-DD
        , end :: String
        , ended :: Bool
        } deriving (Show)

data Rating = EmptyRating | Rating
        { ratingValue :: Double
        , ratingCount :: Int
        } deriving (Show)

data Tag = Tag
        { tagCount :: Int
        , tagName :: String
        } deriving (Show)

instance FromJSON Artist where
        --parseJSON (Object v) | trace ("=> Artist: " ++ show v) False = undefined
        parseJSON (Object v) = Artist <$>
                v .:? "alias-list"      .!= [] <*>
                v .:? "area"            .!= NullArea <*>
                v .:? "begin-area"      .!= NullArea <*>
                v .:? "country"         .!= "" <*>
                v .:? "disambiguation"  .!= "" <*>
                v .:? "end-area"        .!= NullArea <*>
                v .:? "id"              .!= "" <*>
                v .:? "isni-list"       .!= [] <*>
                v .:? "life-span"       .!= NullLifeSpan <*>
                v .:? "name"            .!= ""<*>
                v .:? "rating"          .!= EmptyRating <*>
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
                v .:? "iso_3166_1_code-list"  .!= [] <*>
                v .:? "iso_3166_2_code-list"  .!= [] <*>
                v .:? "iso_3166_3_code-list"  .!= [] <*>
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

-- Field parsing functions
readInt    = fromMaybe (-1) . readMaybe
readDouble = fromMaybe  0.0 . readMaybe
