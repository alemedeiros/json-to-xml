-- ToXml.hs
--  by Alexandre Medeiros <alexandre.n.medeiros _at_ gmail.com>
--     Tom Hedges <t.w.hedges _at_ qmul.ac.uk>
--
-- JSON to XML translation using Aeson
--
-- Internal Datatype to XML converter module

module ToXml where

import Datatypes
import Text.XML.Light
import qualified Data.ByteString.Lazy as BS

--Takes Artist (internal datatype defined in Datatypes.hs) returns XML Element
--Top of the xml tree, contains XML Content from functions below (makeXMLArea, makeXMLAlias etc.)
-- If passed NullArtist, returns an empty element (same for all other makeXml... functions)

makeXmlArtist :: Artist -> Element
makeXmlArtist NullArtist = Element (unqual "Artist") [] [] Nothing
makeXmlArtist artist = Element
                       (unqual "Artist")
                       []
                       [
                         Elem $ Element (unqual "ArtistAliases") [] (map makeXmlAlias $ artistAliases artist) Nothing,
                         Elem $ Element (unqual "ArtistArea") [] [(makeXmlArea $ artistArea artist)] Nothing,
                         Elem $ Element (unqual "ArtistBeginArea") [] [(makeXmlArea $ artistBeginArea artist)] Nothing,
                         Elem $ Element (unqual "ArtistCountry") [] [Text $ CData CDataText (artistCountry artist) Nothing] Nothing,
                         Elem $ Element (unqual "ArtistDisambig") [] [Text $ CData CDataText (artistDisambig artist) Nothing] Nothing,
                         Elem $ Element (unqual "ArtistEndArea") [] [(makeXmlArea $ artistEndArea artist)] Nothing,
                                                  
                         Elem $ Element (unqual "ArtistID") [] [Text $ CData CDataText (artistID artist) Nothing] Nothing,
                         Elem $ Element (unqual "ArtistISNIs") [] (map makeArtistISNI $ artistISNI artist) Nothing,
                         Elem $ Element (unqual "ArtistLifeSpan") [] [(makeXmlLifeSpan $ artistLifeSpan artist)] Nothing,
                         Elem $ Element (unqual "ArtistName") [] [Text $ CData CDataText (artistName artist) Nothing] Nothing,
                         Elem $ Element (unqual "ArtistRating") [] [(makeXmlRating $ artistRating artist)] Nothing,
                         Elem $ Element (unqual "ArtistSortName") [] [Text $ CData CDataText (artistSortName artist) Nothing] Nothing,
                         Elem $ Element (unqual "ArtistTags") [] (map makeXmlTag $ artistTags artist) Nothing,
                         Elem $ Element (unqual "ArtistType") [] [Text $ CData CDataText (artistType artist) Nothing] Nothing
                         ]
                       Nothing

makeArtistISNI :: String -> Content
makeArtistISNI isni = Elem $ Element
                  (unqual "artistISNI")
                  []
                  [Text $ CData CDataText isni Nothing]
                  Nothing

--Artist Alias
--Returns XML Content up to makeArtistXML

makeXmlAlias :: Alias -> Content
makeXmlAlias NullAlias = Elem $ Element (unqual "Alias") [] [] Nothing
makeXmlAlias alias = Elem $ Element
                     (unqual "Alias")
                     []
                     [
                       Elem $ Element (unqual "AliasLocale") [] [Text $ CData CDataText (aliasLocale alias) Nothing] Nothing,
                       Elem $ Element (unqual "AliasName") [] [Text $ CData CDataText (aliasName alias) Nothing] Nothing,
                       Elem $ Element (unqual "AliasPrimary") [] [Text $ CData CDataText (show $ aliasPrimary alias) Nothing] Nothing,
                       Elem $ Element (unqual "AliasSortName") [] [Text $ CData CDataText (aliasSortName alias) Nothing] Nothing,
                       Elem $ Element (unqual "AliasType") [] [Text $ CData CDataText (aliasType alias) Nothing] Nothing
                       ]
                     Nothing

--Artist Area
-- Returns XML Conent up to makeXmlArtist
-- Uses makeAreaISO to get areaISO lists

makeXmlArea :: Area -> Content
makeXmlArea NullArea = Elem $ Element (unqual "Area") [] [] Nothing
makeXmlArea area = Elem $ Element
                   (unqual "Area")
                   []
                   [
                     Elem $ Element (unqual "AreaDisambig") [] [Text $ CData CDataText (areaDisambig area) Nothing] Nothing,
                     Elem $ Element (unqual "AreaID") [] [Text $ CData CDataText (areaID area) Nothing] Nothing,
                     Elem $ Element (unqual "AreaISO1s") [] (map makeAreaISO $ areaISO1 area) Nothing,
                     Elem $ Element (unqual "AreaISO2s") [] (map makeAreaISO $ areaISO2 area) Nothing,
                     Elem $ Element (unqual "AreaISO3s") [] (map makeAreaISO $ areaISO3 area) Nothing,
                     Elem $ Element (unqual "AreaName") [] [Text $ CData CDataText (areaName area) Nothing] Nothing,
                     Elem $ Element (unqual "AreaSortName") [] [Text $ CData CDataText (areaSortName area) Nothing] Nothing
                   ]
                   Nothing

makeAreaISO :: String -> Content
makeAreaISO iso = Elem $ Element
                  (unqual "areaISO")
                  []
                  [Text $ CData CDataText iso Nothing]
                  Nothing

--Artist LifeSpan
--returns XML Conent up to makeXmlArtist 

makeXmlLifeSpan :: LifeSpan -> Content
makeXmlLifeSpan NullLifeSpan = Elem $ Element (unqual "LifeSpan") [] [] Nothing
makeXmlLifeSpan lifeSpan = Elem $ Element
                           (unqual "LifeSpan")
                           []
                           [
                             Elem $ Element (unqual "LifeSpanBegin") [] [Text $ CData CDataText (lifeSpanBegin lifeSpan) Nothing] Nothing,
                             Elem $ Element (unqual "LifeSpanEnd") [] [Text $ CData CDataText (lifeSpanEnd lifeSpan) Nothing] Nothing,
                             Elem $ Element (unqual "LifeSpanEnded") [] [Text $ CData CDataText (show $ lifeSpanEnded lifeSpan) Nothing] Nothing
                             ]
                           Nothing

--Artist Rating
--returns XML Conent up to makeXmlArtist 

makeXmlRating :: Rating -> Content
makeXmlRating EmptyRating = Elem $ Element (unqual "Rating") [] [] Nothing
makeXmlRating rating = Elem $ Element
                       (unqual "Rating")
                       []
                       [
                         Elem $ Element (unqual "RatingValue") [] [Text $ CData CDataText (show $ ratingValue rating) Nothing] Nothing,
                         Elem $ Element (unqual "RatingCount") [] [Text $ CData CDataText (show $ ratingCount rating) Nothing] Nothing
                         ]
                       Nothing

--Artist Tags
--returns XML Conent up to makeXmlArtist 

makeXmlTag :: Tag -> Content
makeXmlTag NullTag = Elem $ Element (unqual "Tag") [] [] Nothing
makeXmlTag tag = Elem $ Element
                       (unqual "Tag")
                       []
                       [
                         Elem $ Element (unqual "TagCount") [] [Text $ CData CDataText (show $ tagCount tag) Nothing] Nothing,
                         Elem $ Element (unqual "TagName") [] [Text $ CData CDataText (tagName tag) Nothing] Nothing
                         ]
                       Nothing

--takes a filepath, returns the file contents.
--for debugging and testing only.

getFileContents :: FilePath -> IO BS.ByteString
getFileContents fileName =
  BS.readFile fileName
                       

