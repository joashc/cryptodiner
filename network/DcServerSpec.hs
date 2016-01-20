module DcServerSpec where

import Control.Lens
import Control.Monad.State
import DcNetwork
import DcServerFree
import Messaging

-- | Defines the server semantics in the 'DcServer' DSL
serverProg :: DcServer ()
serverProg = do
  initServer
  forever $ getMessage >>= messageHandler

-- | Collects a specified number of entities, and then performs an action
collectAndThen :: DcServer () -> DcServer () -> Int -> Int -> DcServer ()
collectAndThen collect action total current = do
  when (total > current) collect
  when (total - 1 == current) action

-- | Dispatch on message type
messageHandler :: ServerMessage -> DcServer ()
-- | Accept peers until we have enough, then send the peer information to all peers
messageHandler (PeerJoin ps) = do
  ss <- getServerState
  let peers = ss^.registeredPeers
  collectAndThen (addPeer ps) (sendBroadcast peers $ PeerListB peers) (ss^.numPeers) (ss^.registeredPeers.to length)
-- | Collects streams until everyone's sent their stream, then send the combined stream to everyone
messageHandler (Stream s) = do
  ss <- getServerState
  let numStreamsReceived = ss^.roundStreams.to length
      requiredNum = ss^.numPeers
  collectAndThen (addStream s) sendRoundResult requiredNum numStreamsReceived

combineStreams :: [RoundStream] -> DcServer RoundStream
combineStreams rs = return $ xorStreams rs

sendRoundResult :: DcServer ()
sendRoundResult = do
  ss <- getServerState
  result <- combineStreams $ ss^.roundStreams
  sendBroadcast (ss^.registeredPeers) (RoundResultB result)
