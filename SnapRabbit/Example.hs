module SnapRabbit.Example 
    ( run
    ) where

import Control.Monad.IO.Class                  ( liftIO )
import SnapRabbit.Server
import SnapRabbit.SimpleProtocol

-- | Simple state to maintain user's name once acknowledged
data ClientState = ClientState
    { clientName :: Maybe String }

initialState :: ClientState
initialState = ClientState Nothing

defaultRun :: IO ()
defaultRun = run defaultSettings

run :: Settings -> IO ()
run settings = runChat settings initialState $ simpleProtocol $ 
    \msg -> do
        case msg of
            ChatMessage Message pl -> do
                -- Ordinary message
                ClientState name <- get
                case name of
                    Nothing -> 
                        -- The user has not been acknowledged
                        void
                    Just n  -> 
                        -- Prepend user name and send message
                        broadcast $ n ++ ": " ++ pl

            ChatMessage Acknowledge pl ->  do
                -- Set user's login name
                put $ ClientState $ Just pl
                broadcast $ pl ++ " has joined the chat."

            NoMessage -> do
                -- JSON parsing failed -- not a valid message object
                liftIO $ print "Invalid message format."
                void

