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
  sayString "Waiting for peers..."
  peers <- awaitFullPeerList
  sayString "All peers joined."
  sendBroadcast peers $ PeerListB peers

listenForMessages :: DcServer()
listenForMessages = forever $ getMessage >>= messageHandler

messageHandler :: ServerMessage -> DcServer ()
messageHandler (PeerJoin ps) = modifyState $ addPeerIfNeeded ps
messageHandler (Stream s) = modifyState $ addStreamIfNeeded s

combineStreams :: [RoundStream] -> DcServer RoundStream
combineStreams rs = return $ xorStreams rs

sendRoundResult :: DcServer ()
sendRoundResult = do
  ss <- getServerState
  result <- combineStreams $ ss^.roundStreams
  sendBroadcast (ss^.registeredPeers) (RoundResultB result)

awaitFullPeerList :: DcServer [Participant]
awaitFullPeerList = do
  state <- awaitStateCondition (\s -> s ^. numPeers == s ^. registeredPeers . to length)
  return $ state ^. registeredPeers

addStreamIfNeeded :: RoundStream -> ServerState -> ServerState
addStreamIfNeeded stream s =
  if numStreams < needed then s & roundStreams %~ (:) stream else s
  where numStreams = s^.roundStreams.to length
        needed = s ^. numPeers

addPeerIfNeeded :: Participant -> ServerState -> ServerState
addPeerIfNeeded p s =
  if peerCount < needed then s & registeredPeers %~ (:) p else s
  where peerCount = s ^. registeredPeers . to length
        needed = s ^. numPeers

