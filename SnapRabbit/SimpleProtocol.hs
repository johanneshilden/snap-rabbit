{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module SnapRabbit.SimpleProtocol 
    ( module Control.Monad.Trans.State.Lazy
    , ChatMessage(..)
    , ChatMsgType(..)
    , broadcast
    , simpleProtocol
    , void
    ) where

import Control.Applicative                     ( (<$>)
                                               , (<*>) )
import Control.Monad                           ( mzero )
import Control.Monad.IO.Class                  ( liftIO )
import Control.Monad.Trans.State.Lazy          ( StateT, get, put )
import Data.Aeson
import Data.ByteString.Lazy                    ( ByteString )
import Data.Text
import SnapRabbit.Server

import qualified Data.ByteString.Lazy.Char8    as BL

data ChatMsgType = Message          -- ^ Ordinary message
                 | Acknowledge      -- ^ New user

data ChatMessage 
   = NoMessage 
   | ChatMessage { chatMsgType    :: ChatMsgType
                 , chatMsgPayload :: String
                 }

instance FromJSON ChatMsgType where
    parseJSON (String "message")     = return Message
    parseJSON (String "acknowledge") = return Acknowledge
    parseJSON _ = mzero

instance FromJSON ChatMessage where
    parseJSON (Object v) = 
        ChatMessage <$> v .: "type"
                    <*> v .: "payload"
    parseJSON _ = mzero

instance ToJSON ChatMsgType where
    toJSON Message     = String "message"
    toJSON Acknowledge = String "acknowledge"

instance ToJSON ChatMessage where
    toJSON ChatMessage{..} =
        object [ "type"    .= chatMsgType
               , "payload" .= chatMsgPayload ]

simpleProtocol :: MsgBuilder a ChatMessage 
               -> MsgBuilder a ByteString
simpleProtocol s = \raw -> 
    case decode raw of
      Just msg -> s msg
      Nothing  -> return $ Nothing

void :: StateT a IO (Maybe ByteString)
void = return Nothing

broadcast :: String -> StateT a IO (Maybe ByteString)
broadcast = return . Just . BL.pack 

