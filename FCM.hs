{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}


module FCM (
  fcmSendItem
) where


import           Args
import           Control.Lens
import           Data.Default
import qualified Data.Map as Map
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
itemMessage args@Args{..} i@Item{..} =
  def & fcmTo ?~ argFcmTo
      & fcmNotification ?~ itemNotification args i
      & fcmData ?~ Map.fromList [("text", itemTitle <> itemDescription)]
      & fcmTimeToLive ?~ 3600 -- 1h TTL


itemNotification :: Args
                 -> Item
                 -> FCMNotification
itemNotification Args{..} Item{..} =
  def & fcmTitle ?~ "NJT " <> itemTitle
      & fcmBody ?~ itemDescription
      & fcmTag ?~ itemGuid
      & fcmColor .~ argFcmColor
      & fcmIcon .~ argFcmIcon
