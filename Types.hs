{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module Types (
  Item(..)
) where


import Data.Text (Text)
import Data.Time.Clock


data Item =
  Item { itemTitle :: Text
       , itemDescription :: Text
       , itemLink :: Text
       , itemGuid :: Text
       , itemPubDate :: UTCTime
       } deriving (Show)


-- | Equality by Guid
instance Eq Item where
  i1 == i2 = itemGuid i1 == itemGuid i2


-- | Ordering by pub date
instance Ord Item where
  compare i1 i2 = compare (itemPubDate i1) (itemPubDate i2)
