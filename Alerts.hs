{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module Alerts (
  filterAlertItems
, sortAlertItems
) where

import           Args
import           Data.Foldable
import           Data.List
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Time.Clock
import           Types


sortAlertItems :: (Foldable f)
               => f Item
               -> [Item]
sortAlertItems = sortOn itemPubDate . toList


filterAlertItems :: Args
                 -> Set Item
                 -> IO (Set Item)
filterAlertItems Args{..} !is = do
  tNow <- getCurrentTime
  let cutoffTime = addUTCTime argLookBackTime tNow
      selLineSubstr = "selLine=" <> argNJTLine <> "#"
      trainSubstrs = (("#" <>) . T.pack . show) <$> argNJTTrains
      anyTrain s = any (`T.isInfixOf` s) trainSubstrs

      p Item{..} = (itemPubDate >= cutoffTime) &&
                     T.isInfixOf selLineSubstr itemLink &&
                     (anyTrain itemTitle || anyTrain itemDescription)

  return $ Set.filter p is
