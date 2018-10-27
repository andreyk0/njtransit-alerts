{-# LANGUAGE OverloadedStrings  #-}

module ParseXMLFeed (
  parseXMLFeed
) where

import qualified Data.ByteString.Lazy as L
import           Data.Maybe
import qualified Data.Text as T
import           Data.Time.Format
import           Text.XML
import           Text.XML.Lens
import           Types


parseXMLFeed :: L.ByteString
             -> Either String [Item]
parseXMLFeed bs =
  let doc = parseLBS_ def bs
      itemElems = doc ^.. root . el "rss" ./ el "channel" ./ el "item"
      items = mapMaybe parseElem itemElems
   in if null itemElems
      then Left $ "Unable to parse any items from " <> show doc
      else if length itemElems /= length items
           then Left $ "Unable to parse all items from " <> show itemElems
           else Right items



parseElem :: Element
          -> Maybe Item
parseElem e = Item
  <$> t "title"
  <*> t "description"
  <*> t "link"
  <*> t "guid"
  <*> ( do pdTxt <- t "pubDate"
           parsePubDate pdTxt )

  where t n = e ^? el "item" ./ el n . text
        parsePubDate tstr = parseTimeM True defaultTimeLocale "%b %d, %Y %H:%M:%S %p" (T.unpack tstr)
