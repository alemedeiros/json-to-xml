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
        { name :: Text
        , age :: Int
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
                v .: "age"
        -- A non-Object value is of the wrong type, so fail.
        parseJSON _ = mzero

instance ToJSON Person where
        toJSON (Person name age) = object ["name" .= name, "age" .= age]

test1 :: ByteString
test1 = "{\"name\":\"Joe\",\"age\":12}"

aesonSample :: Maybe Person
aesonSample = decode test1
