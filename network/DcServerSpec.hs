module DcServerSpec where

import Control.Lens
import Control.Monad.State
import DcNetwork
import DcServerFree
import Messaging
import DcNodeOperator

-- | Defines the server semantics in the 'DcServer' DSL
serverProg :: DcServer ()
serverProg = do
  initState
  displayMessage "Waiting for peers..."
  peers <- awaitFullPeerList
  displayMessage "All peers joined."
  sendOutgoing peers $ PeerListB peers
  forever $ awaitAllStreams >>= sendRoundResult

listenForMessages :: DcServer()
listenForMessages = forever $ getIncoming >>= messageHandler

messageHandler :: ServerMessage -> DcServer ()
messageHandler (PeerJoin ps) = modifyState $ addPeerIfNeeded ps
messageHandler (Stream s) = modifyState $ addStreamIfNeeded s

combineStreams :: [RoundStream] -> DcServer RoundStream
combineStreams rs = return $ xorStreams rs

sendRoundResult :: [RoundStream] -> DcServer ()
sendRoundResult rs = do
  result <- combineStreams rs
  state <- getState
  modifyState $ roundStreams .~ []
  sendOutgoing (state^.registeredPeers) (RoundResultB result)

awaitFullPeerList :: DcServer [Participant]
awaitFullPeerList = do
  state <- awaitStateCondition (\s -> s ^. numPeers == s ^. registeredPeers . to length)
  return $ state ^. registeredPeers

awaitAllStreams :: DcServer [RoundStream]
awaitAllStreams = do
  state <- awaitStateCondition (\s -> s ^. numPeers == s ^. roundStreams .to length)
  return $ state ^. roundStreams

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
