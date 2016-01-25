{-# LANGUAGE DeriveFunctor, TemplateHaskell, FlexibleContexts #-}
module DcPeerFree where

import Control.Monad.Free.TH
import Data.ByteString as B (ByteString)
import Control.Monad.Free
import Control.Lens
import Messaging
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
  _reservation :: Maybe Int,
  _peerSocket :: Socket,
  _roundResult :: Maybe RoundStream,
  _cachedMessage :: Either String String
} deriving (Show)

-- Automagic some lenses with TH
makeLenses ''PeerState

data PeerError = StreamGenerationError String | ReservationError String | PeerSocketError | ServerDisconnected | ServerTimeout | InvalidPeerState deriving (Show)

-- | Peer operations
type DcPeerOperator = DcNodeOperator PeerState Broadcast ServerMessage () PeerError

-- | The free monad over 'DcPeerOperator'
type DcPeer = Free DcPeerOperator

numPeers :: PeerState -> Int
numPeers s = s ^. peers . to length

nonces :: PeerState -> [B.ByteString]
nonces s = s ^. peers . to (map peerNonce)
