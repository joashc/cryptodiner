{-# LANGUAGE DeriveFunctor, TemplateHaskell, FlexibleContexts #-}
module DcPeerFree where

import Control.Monad.Free.TH
import Control.Monad.Free
import Control.Lens
import Messaging
import Data.ByteString as B
import DiffieHellman
import Network (PortNumber, Socket)
import DcNodeOperator

-- | Represents the peer state. Initialized by the 'InitState' operation.
data PeerState = PeerState {
  _privateKey :: PrivateKey,
  _peers :: [Participant],
  _roundNum :: Int,
  _ownNonce :: Nonce,
  _listenPort :: Int,
  _peerSocket :: Socket
} deriving (Show)

-- Automagic some lenses with TH
makeLenses ''PeerState

data PeerError = PeerSocketError | ServerDisconnected | ServerTimeout | InvalidPeerState deriving (Show)

-- | Peer operations
type DcPeerOperator = DcNodeOperator PeerState Broadcast ServerMessage () PeerError

-- | The free monad over 'DcPeerOperator'
type DcPeer = Free DcPeerOperator
