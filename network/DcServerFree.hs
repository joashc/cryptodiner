{-# LANGUAGE DeriveFunctor, TemplateHaskell #-}
module DcServerFree where

import Control.Monad.Free
import Control.Lens
import Messaging
import Data.ByteString as B
import Network (Socket)
import Control.Concurrent.STM

-- | Represents the server state. Initialized by the 'InitServer' operation.
data ServerState = SS {
  _numPeers :: Int,
  _registeredPeers :: [Participant],
  _roundStreams :: [RoundStream],
  _listenSocket :: Maybe Socket
}

-- Automagic some lenses with TH
makeLenses ''ServerState

-- | Defines the DSL for the server
data DcServerOperator next =
    InitServer next
  | GetMessage (ServerMessage -> next)
  | AwaitStateCondition (ServerState -> Bool) (ServerState -> next)
  | SendBroadcast [Participant] Broadcast next
  | GetServerState (ServerState -> next)
  | ModifyState (ServerState -> ServerState) next
  | SayString String next
  | Throw ServerError next
  deriving (Functor)

-- | The free monad over 'DcServerOperator'
type DcServer = Free DcServerOperator

-- | Possible errors
data ServerError = BadPeerState | PeerDisconnected | Timeout | SocketError deriving (Show)

-- Boilerplate functions for the server operators
initServer :: DcServer ()
initServer = liftF $ InitServer ()

modifyState :: (ServerState -> ServerState) -> DcServer()
modifyState f = liftF $ ModifyState f ()

getMessage :: DcServer ServerMessage
getMessage = liftF $ GetMessage id

sendBroadcast :: [Participant] -> Broadcast -> DcServer ()
sendBroadcast ps b = liftF $ SendBroadcast ps b ()

sayString :: String -> DcServer ()
sayString s = liftF $ SayString s ()

getServerState :: DcServer ServerState
getServerState = liftF $ GetServerState id

awaitStateCondition :: (ServerState -> Bool) -> DcServer ServerState
awaitStateCondition cond = liftF $ AwaitStateCondition cond id

throw :: ServerError -> DcServer ()
throw err = liftF $ Throw err ()

