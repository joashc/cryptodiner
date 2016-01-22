module DcPeerIO where

import DcServerSpec
import Network
import DhGroupParams
import System.IO
import Messaging
import DiffieHellman
import DcPeerFree
import DcPeerSpec
import Control.Lens
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Free
import Text.Read (readMaybe)
import RandomBytes

pk = PublicKey 589430589430 (GroupParameters 5834759 753489573849)

peer = Participant pk (strBytes "5353") "localhost" 5435

-- Here's the type our peer interpreter is going to map the free monad to
type DcPeerIO = ExceptT PeerError (StateT PeerState IO)

getPortNumber :: IO Int
getPortNumber = try
  where
    try = do
      putStrLn "Listen port:"
      p <- getLine
      case (readMaybe p :: Maybe Int) of
        Nothing -> putStrLn "Couldn't parse port number, try again." >> try
        Just num -> return num

peerIOInterpreter :: DcPeerOperator (DcPeerIO next) -> DcPeerIO next
peerIOInterpreter (InitPeer next) = do
  -- Generate private key
  randNum <- liftIO $ systemRandomNum 768
  privateKey .= (Just $ PrivateKey randNum gp)
  liftIO $ putStrLn "Generated private key"
  -- Generate nonce
  randBytes <- liftIO $ systemRandomBytes 256
  ownNonce .= Just randBytes
  -- Get port number
  port <- liftIO getPortNumber
  listenPort .= Just port
  -- Start listening on port
  socket <- liftIO $ listenOn $ PortNumber (toEnum port)
  peerSocket .= Just socket
  next
peerIOInterpreter (SendMessage m next) = do
  liftIO $ putStrLn "Sending MESSAGE"
  liftIO $ sendToServer m
  next
peerIOInterpreter (UpdatePeerList ps next) = do
  liftIO $ putStrLn "Updating peer list"
  peers .= ps
  liftIO $ print ps
  next
peerIOInterpreter (ReceiveBroadcast next) = do
  ps <- get
  broadcast <- listenForBroadcast $ ps^.peerSocket
  next broadcast
peerIOInterpreter (GetPeerState next) = get >>= next
peerIOInterpreter (PeerThrow err next) = throwError err >> next

peerIO :: DcPeer a -> DcPeerIO a
peerIO = iterM peerIOInterpreter

reportResult (Right a) = putStrLn "SUCCESS!"
reportResult (Left a) = print a

-- | Run our Program
runPeer = do
  (r, _) <- runStateT (runExceptT (peerIO peerProg)) initialPeerState
  reportResult r

listenForBroadcast :: Maybe Socket -> DcPeerIO Broadcast
listenForBroadcast Nothing = throwError PeerSocketError
listenForBroadcast (Just s) = liftIO fetch
  where fetch = do {
    (handle, addr, portNum) <- accept s;
    c <- hGetContents handle;
    case decodeBroadcast c of
      Left e -> putStrLn ("Error parsing message: " ++ e) >> fetch
      Right m -> return m
}
