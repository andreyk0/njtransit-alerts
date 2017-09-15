{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module Main where

import           Alerts
import           Args
import           Control.Concurrent
import           Control.Monad
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as Set
import           FCM
import           Network.HTTP.Conduit
import           ParseXMLFeed
import           Types


main:: IO ()
main = runWithArgs $ \args@Args{..} -> do
  let actions = repeat (refreshAlertItems  args)

  -- Don't bother catching exceptions, watchdog process will restart
  foldM_ (\items act -> do
             newItems <- act items
             threadDelay argRefreshIntervalMicroseconds
             return newItems
         ) Set.empty actions


refreshAlertItems :: Args
                  -> Set Item
                  -> IO (Set Item)
refreshAlertItems !args !oldItems = do
  putStrLn "Refreshing alert items ..."

  newFeed <- fetchAlertItems >>= filterAlertItems args
  let newItems = Set.difference newFeed oldItems

  putStrLn $ "Sending alerts for " <> (show . Set.size) newItems <> "items ..."
  forM_ (sortAlertItems newItems) $ \i -> fcmSendItem args i

  -- Keep all recent items around for de-duping
  filterAlertItems args $ Set.union newItems oldItems


fetchAlertItems :: IO (Set Item)
fetchAlertItems = do
  hRes <- simpleHttp "http://www.njtransit.com/rss/RailAdvisories_feed.xml"
  case parseXMLFeed hRes
    of Left e -> error e
       Right is -> return $ Set.fromList is
