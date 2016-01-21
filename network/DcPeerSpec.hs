module DcPeerSpec where

import Control.Lens
import Control.Monad.State
import DcNetwork
import DcPeerFree
import Messaging
import DiffieHellman

-- | Defines the peer semantics in the 'DcPeer' DSL
peerProg :: DcPeer ()
peerProg = do
  initPeer
  ps <- getPeerState
  sendParticipantInfo $ participantInfo ps
  forever $ receiveBroadcast >>= broadcastHandler

-- | Dispatch on message type
broadcastHandler :: Broadcast -> DcPeer ()
broadcastHandler (PeerListB ps) = updatePeerList ps
broadcastHandler (RoundResultB r) = displayResult r

participantInfo :: PeerState -> Maybe Participant
participantInfo ps = do
  priv <- ps^.privateKey
  nonce <- ps^.ownNonce
  port <- ps^.listenPort
  let pub = calculatePublicKey priv
  return $ Participant pub nonce "localhost" port

sendParticipantInfo :: Maybe Participant -> DcPeer ()
sendParticipantInfo Nothing = peerThrow InvalidPeerState
sendParticipantInfo (Just p) = sendMessage $ PeerJoin p


