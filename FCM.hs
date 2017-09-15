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
  print res -- try to send, ignore errs


itemMessage :: Args
            -> Item
            -> FCMMessage
itemMessage args@Args{..} i@Item{..} = (
    (fcmTo .~ Just argFcmTo) .
    (fcmNotification .~ (Just $ itemNotification args i)) .
    (fcmData .~ (Just . Map.fromList) [("text", itemTitle <> itemDescription)]) .
    (fcmTimeToLive .~ Just 3600) -- 1h TTL
  ) def


itemNotification :: Args
                 -> Item
                 -> FCMNotification
itemNotification Args{..} Item{..} = (
    (fcmTitle .~ (Just $ "NJT " <> itemTitle)) .
    (fcmBody .~ Just itemDescription) .
    (fcmTag .~ Just itemGuid) .
    (fcmColor .~ argFcmColor) .
    (fcmIcon .~ argFcmIcon)
  ) def
