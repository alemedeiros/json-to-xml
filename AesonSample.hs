{-# LANGUAGE OverloadedStrings #-}

-- The above is a "Haskell extension"
-- See https://www.fpcomplete.com/school/to-infinity-and-beyond/pick-of-the-week/guide-to-ghc-extensions/basic-syntax-extensions
-- for an explanation of what GHC extensions do, and what "OverloadedStrings" in particular does

module AesonSample where

import Data.Aeson
import Data.Text
import Data.Functor
import Data.ByteString.Lazy
import Control.Applicative
import Control.Monad

data Person = Person
        { n :: Text
        , a :: [Int]
        , b :: Date
        } deriving Show

data Date = NoDate | Date
        { y :: Int
        , m :: Int
        , d :: Int
        } deriving Show

-- The following makes use of <$> :: Functor f => (a -> b) -> f a -> f b, an infix notation for "fmap", see
--
-- https://hackage.haskell.org/package/base-4.6.0.0/docs/Data-Functor.html
--
-- and (<*>) :: f (a -> b) -> f a -> f b, see
--
-- https://hackage.haskell.org/package/base-4.6.0.0/docs/Control-Applicative.html
--
-- See also Aeson specific functions
--
-- (.:) :: FromJSON a => Object -> Text -> Parser a
-- (.=) :: ToJSON a => Text -> a -> Pair

instance FromJSON Person where
        parseJSON (Object v) = Person <$>
                v .: "name" <*>
                v .: "age" <*>
                v .:? "birth" .!= NoDate
        -- A non-Object value is of the wrong type, so fail.
        parseJSON _ = mzero

instance FromJSON Date where
        parseJSON (Object v) = Date <$>
                v .: "year" <*>
                v .: "month" <*>
                v .: "day"
        parseJSON Null = Control.Applicative.empty
        parseJSON _ = mzero

instance ToJSON Person where
        toJSON (Person name age birth) = object ["name" .= name, "age" .= age, "birth" .= birth]

instance ToJSON Date where
        toJSON (Date year month day) = object ["year" .= year, "month" .= month, "day" .= day]

test1 :: ByteString
test1 = "{\n  \"name\":\"Joe\",\n  \"age\": [12], \n  \"birth\": {\"year\":1992,\"month\":7, \"day\":14}}"

test2 :: ByteString
test2 = "{\"name\":\"Joe\",\"age\": [12,10,9], \"birth\": null}"

test3 :: ByteString
test3 = "{\"name\":\"Joe\",\"age\": null, \"birth\": null}"

aesonSample :: Maybe Person
aesonSample = decode test3
