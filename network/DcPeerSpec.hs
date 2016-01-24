module DcPeerSpec where

import Control.Lens
import Control.Monad.State
import DcNetwork
import DcPeerFree
import Messaging
import DiffieHellman
import RandomBytes
import DcNodeOperator

-- | Defines the peer semantics in the 'DcPeer' DSL
peerProg :: DcPeer ()
peerProg = do
  initState
  ps <- getState
  sendOutgoing () $ PeerJoin (participantInfo ps)
  plaintext <- getUserInput
  peers <- awaitPeers
  sendOutgoing () $ Stream (strBytes plaintext)

-- | Dispatch on message type
broadcastHandler :: Broadcast -> DcPeer ()
broadcastHandler (PeerListB ps) = updatePeerList ps
broadcastHandler (RoundResultB r) = displayMessage $ show r

awaitPeers :: DcPeer [Participant]
awaitPeers = do
  state <- awaitStateCondition $ \s -> s ^. peers . to length > 1
  return $ state ^. peers

listenForBroadcasts :: DcPeer ()
listenForBroadcasts = forever $ getIncoming >>= broadcastHandler

participantInfo :: PeerState -> Participant
participantInfo ps = Participant pub (ps^.ownNonce) "localhost" (ps^.listenPort)
  where pub = calculatePublicKey $ ps^.privateKey

updatePeerList :: [Participant] -> DcPeer ()
updatePeerList p = modifyState $ \s -> s & peers .~ p
