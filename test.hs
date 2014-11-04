{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid
import qualified Data.ByteString.Lazy as BSL
import Text.XML.Generator -- the main xmlgen module

genPersonElem :: (String, String) -> Xml Elem
genPersonElem (name, age) =
  xelem "person" $ xattr "age" age <#> xtext name
