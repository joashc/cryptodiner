{-# LANGUAGE DeriveFunctor, TemplateHaskell #-}
module DcPeerFree where

import Control.Monad.Free
import Control.Lens
import Messaging
import Data.ByteString as B
import DiffieHellman
import Network (PortNumber, Socket)

-- | Represents the peer state. Initialized by the 'InitPeer' operation.
data PeerState = PeerState {
  _privateKey :: Maybe PrivateKey,
  _peers :: [Participant],
  _roundNum :: Int,
  _ownNonce :: Maybe Nonce,
  _listenPort :: Maybe Int,
  _peerSocket :: Maybe Socket
} deriving (Show)

initialPeerState = PeerState Nothing [] 0 Nothing Nothing Nothing

-- Automagic some lenses with TH
makeLenses ''PeerState

-- | Defines the DSL for the peer
data DcPeerOperator next =
    InitPeer next
  | SendMessage ServerMessage next
  | ReceiveBroadcast (Broadcast -> next)
  | GetPeerState (PeerState -> next)
  | PeerThrow PeerError next
  | DisplayResult RoundStream next
  | UpdatePeerList [Participant] next
  deriving (Functor)

-- | The free monad over 'DcPeerOperator'
type DcPeer = Free DcPeerOperator

data PeerError = PeerSocketError | ServerDisconnected | ServerTimeout | InvalidPeerState deriving (Show)

-- Boilerplate functions for the peer operators
initPeer :: DcPeer ()
initPeer = liftF $ InitPeer ()

sendMessage :: ServerMessage -> DcPeer ()
sendMessage m = liftF $ SendMessage m ()

receiveBroadcast :: DcPeer Broadcast
receiveBroadcast = liftF $ ReceiveBroadcast id

getPeerState :: DcPeer PeerState
getPeerState = liftF $ GetPeerState id

updatePeerList :: [Participant] -> DcPeer ()
updatePeerList ps = liftF $ UpdatePeerList ps ()

displayResult :: RoundStream -> DcPeer ()
displayResult r = liftF $ DisplayResult r ()

peerThrow :: PeerError -> DcPeer ()
peerThrow e = liftF $ PeerThrow e ()
