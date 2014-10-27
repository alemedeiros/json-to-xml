-- Datatypes.hs
--  by alemedeiros <alexandre.n.medeiros _at_ gmail.com>

{-# LANGUAGE OverloadedStrings #-}

module Datatypes where

import Control.Applicative
import Control.Monad

import Data.Aeson

data Artist = Artist
        { artistAliases :: [Alias]
        , artistArea :: Area
        , artistBeginArea :: Area
        , artistCountry :: String
        , artistDisambig :: String
        , artistEndArea :: Area
        , artistID :: String
        , artistIPIS :: [String]
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
        { begin :: String -- Change to a date or something like YYYY-MM-DD
        , end :: String
        , ended :: Bool
        } deriving (Show)

data Rating = EmptyRating | Rating
        { ratingValue :: Double -- may be null
        , ratingCount :: Int
        } deriving (Show)

data Tag = Tag
        { tagCount :: Int
        , tagName :: String
        } deriving (Show)

instance FromJSON Artist where
        parseJSON (Object v) = Artist <$>
                v .:? "aliases"           .!= [] <*>
                v .:? "area"              .!= NullArea <*>
                v .:? "begin_area"        .!= NullArea <*>
                v .:? "country"           .!= "" <*>
                v .:? "disambiguation"    .!= "" <*>
                v .:? "end_area"          .!= NullArea <*>
                v .:? "id"                .!= "" <*>
                v .:? "ipis"              .!= [] <*>
                v .:? "lige-span"         .!= NullLifeSpan <*>
                v .:? "name"              .!= ""<*>
                v .:? "rating"            .!= EmptyRating <*>
                v .:? "sort-name"         .!= "" <*>
                v .:? "tags"              .!= [] <*>
                v .:? "type"              .!= ""
        parseJSON _ = fail "fail at Artist"

instance FromJSON Alias where
        parseJSON (Object v) = Alias <$>
                v .:? "locale"            .!= "" <*>
                v .:? "name"              .!= "" <*>
                v .:? "primary"           .!= False <*>
                v .:? "sort-name"         .!= "" <*>
                v .:? "type"              .!= ""
        parseJSON _ = fail "fail at Alias"

instance FromJSON Area where
        parseJSON (Object v) = Area <$>
                v .:? "disambiguation"    .!= "" <*>
                v .:? "id"                .!= "" <*>
                v .:? "iso_3166_1_codes"  .!= [] <*>
                v .:? "iso_3166_2_codes"  .!= [] <*>
                v .:? "iso_3166_3_codes"  .!= [] <*>
                v .:? "name"              .!= "" <*>
                v .:? "sort-name"         .!= ""
        parseJSON _ = fail "fail at Area"

instance FromJSON LifeSpan where
        parseJSON (Object v) = LifeSpan <$>
                v .:? "begin"             .!= "" <*>
                v .:? "end"               .!= "" <*>
                v .:? "ended"             .!= False
        parseJSON _ = fail "fail at LifeSpan"

instance FromJSON Rating where
        parseJSON (Object v) = Rating <$>
                v .:? "value"             .!= 0.0 <*>
                v .:? "votes-count"       .!= 0
        parseJSON _ = fail "fail at Rating"

instance FromJSON Tag where
        parseJSON (Object v) = Tag <$>
                v .:? "count"             .!= 0 <*>
                v .:? "name"              .!= ""
        parseJSON _ = fail "fail at Tag"
