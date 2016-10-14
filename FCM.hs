{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}


module FCM (
  fcmSendItem
) where


import           Args
import           Control.Lens
import           Data.Default
import qualified Data.Map as Map
import           Data.Monoid
import qualified Data.Text.Encoding as E
import           FCMClient
import           FCMClient.Types
import           Types


fcmSendItem :: Args
            -> Item
            -> IO ()
fcmSendItem args@Args{..} i = do
  let m = itemMessage args i
  res <- fcmCallJSON (E.encodeUtf8 argAuthKey) m
  putStrLn $ show res -- try to send, ignore errs


itemMessage :: Args
            -> Item
            -> FCMMessage
itemMessage Args{..} i@Item{..} = (
    (fcmTo .~ (Just argFcmTo)) .
    (fcmTimeToLive .~ (Just 3600)) .
    (fcmNotification .~ (Just $ itemNotification i)) .
    (fcmData .~ (Just . Map.fromList) [("text", itemTitle <> itemDescription)]) .
    (fcmTimeToLive .~ (Just 3600)) -- 1h TTL
  ) def


itemNotification :: Item
                 -> FCMNotification
itemNotification Item{..} = (
    (fcmTitle .~ (Just $ "NJT " <> itemTitle)) .
    (fcmBody .~ (Just itemDescription)) .
    (fcmTag .~ (Just itemGuid))
  ) def
