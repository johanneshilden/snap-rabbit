{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module SnapRabbit.Server
    ( MsgBuilder
    , Settings(..)
    , defaultSettings
    , runChat
    , runChatDefault
    ) where

import Control.Monad.IO.Class                  ( liftIO )
import Control.Monad.Trans.State.Lazy
import Data.ByteString.Lazy
import Data.Monoid                             ( mempty )
import Data.Text                               ( Text )
import Network.AMQP                            ( Ack
                                               , DeliveryMode(..)
                                               , bindQueue
                                               , consumeMsgs
                                               , declareQueue
                                               , deleteQueue
                                               , newMsg
                                               , newQueue
                                               , publishMsg 
                                               , queueExclusive )
import Network.WebSockets
import Network.WebSockets.Snap
import Snap.Core
import Snap.Http.Server

import qualified Data.ByteString.Char8         as BL
import qualified Data.Text                     as Text
import qualified Network.AMQP                  as AMQP

type MsgBuilder s a = a -> StateT s IO (Maybe ByteString)

messageCallback :: Connection -> (AMQP.Message, AMQP.Envelope) -> IO ()
messageCallback ws (msg, env) = do
    print msg
    sendDataMessage ws $ Text $ AMQP.msgBody msg
    -- Acknowledge receiving the message
    AMQP.ackEnv env

processSocket :: MsgBuilder s ByteString   -- ^ Message builder function
              -> Connection                -- ^ WebSocket client connection
              -> AMQP.Channel              -- ^ RabbitMQ channel
              -> Text                      -- ^ A message queue identifier
              -> StateT s IO ()
processSocket builder ws chan queue = do
    msg <- liftIO $ do
        msg <- receive ws
        print msg
        return msg
    case msg of
        DataMessage (Text bs) -> do
            -- Process message using client-supplied builder function
            b <- builder bs 
            case b of
                Just res -> do
                    liftIO $ publishMsg chan "chat-exchange" "chat-key" 
                        newMsg { AMQP.msgBody         = res
                               , AMQP.msgDeliveryMode = Just Persistent
                               }
                Nothing -> 
                    return ()
            loop
        DataMessage (Binary _) -> 
            -- Binary data messages are ignored
            loop
        ControlMessage (Close _) -> 
            -- Control message to close connection
            liftIO $ deleteQueue chan queue >> return ()
        _ -> loop
  where 
    loop = processSocket builder ws chan queue

data Settings = Settings
    -- RabbitMQ connection settings
    { amqpHostname       :: !String
    , amqpVirtualHost    :: !String
    , amqpLoginName      :: !String
    , amqpPassword       :: !String
    -- Snap http server settings
    , bindAddress        :: !String
    , port               :: !Int
    }

defaultSettings :: Settings
defaultSettings = Settings
    { amqpHostname    = "127.0.0.1"
    , amqpVirtualHost = "/"
    , amqpLoginName   = "guest"
    , amqpPassword    = "guest"
    , bindAddress     = "127.0.0.1"
    , port            = 8000
    }

-- | Run the chat server with default settings.
runChatDefault :: s -> MsgBuilder s ByteString -> IO ()
runChatDefault = runChat defaultSettings

-- | Run the chat server with the provided Settings object.
runChat :: Settings                 -- ^ Settings object
        -> s                        -- ^ A state object
        -> MsgBuilder s ByteString  -- ^ Message builder function
        -> IO ()
runChat Settings{..} state builder = do
    -- Open a connection to the AMQP server.
    conn <- AMQP.openConnection amqpHostname 
                               (Text.pack amqpVirtualHost)
                               (Text.pack amqpLoginName)
                               (Text.pack amqpPassword)
    chan <- AMQP.openChannel conn
 
    let config = setBind (BL.pack bindAddress)
               $ setPort port defaultConfig

    httpServe config $ runWebSocketsSnap $ \pc -> do
        -- Upgrade connection to WebSocket protocol.
        ws <- acceptRequest pc

        -- Create a distinct message queue for this connection.
        (queue, _, _) <- declareQueue chan $ newQueue { queueExclusive = True }

        -- The routing key binds the new queue to the exchange.
        bindQueue chan queue "chat-exchange" "chat-key"
        consumeMsgs chan queue AMQP.Ack (messageCallback ws)

        runStateT (processSocket builder ws chan queue) state
        return ()

    -- Close connection to ensure that all published messages are 
    -- received by the server.
    AMQP.closeConnection conn

