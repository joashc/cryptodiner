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

messageHandler :: ServerMessage -> DcServer ()
messageHandler (PeerJoin ps) = addPeer ps
messageHandler (Stream s) = addStream s

combineStreams :: [RoundStream] -> DcServer RoundStream
combineStreams rs = return $ xorStreams rs

sendRoundResult :: DcServer ()
sendRoundResult = do
  ss <- getServerState
  result <- combineStreams $ ss^.roundStreams
  sendBroadcast (ss^.registeredPeers) (RoundResultB result)
