{-# LANGUAGE DeriveFunctor, TemplateHaskell, FlexibleContexts #-}
module DcServerFree where

import Control.Monad.Free
import Control.Lens
import Messaging
import Data.ByteString as B
import Network (Socket)
import Control.Concurrent.STM
import Control.Monad.Free.TH
import DcNodeOperator

-- | Represents the server state. Initialized by the 'InitState' operation.
data ServerState = SS {
  _numPeers :: Int,
  _registeredPeers :: [Participant],
  _roundStreams :: [RoundStream],
  _listenSocket :: Maybe Socket
}

-- Automagic some lenses with TH
makeLenses ''ServerState

-- | Error type
data ServerError = BadPeerState | PeerDisconnected | Timeout | SocketError deriving (Show)

-- | Server operations
type DcServerOperator = DcNodeOperator ServerState ServerMessage Broadcast [Participant] ServerError

-- | The free monad over 'DcServerOperator'
type DcServer = Free DcServerOperator
