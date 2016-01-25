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
import Control.Concurrent.STM
import Control.Concurrent
import DcNodeOperator

pk = PublicKey 589430589430 (GroupParameters 5834759 753489573849)

peer = Participant pk (strBytes "5353") "localhost" 5435

-- Here's the type our peer interpreter is going to map the free monad to
type DcPeerIO = ExceptT PeerError (StateT (TMVar PeerState) IO)

getPortNumber :: IO Int
getPortNumber = try
  where
    try = do
      putStrLn "Listen port:"
      p <- getLine
      case (readMaybe p :: Maybe Int) of
        Nothing -> putStrLn "Couldn't parse port number, try again." >> try
        Just num -> return num

readPeerState :: DcPeerIO PeerState
readPeerState = do
  state <- get
  liftIO . atomically $ readTMVar state

peerIOInterpreter :: DcPeerOperator (DcPeerIO next) -> DcPeerIO next
peerIOInterpreter (InitState next) = do
  state <- get
  -- Generate private key
  randNum <- liftIO $ systemRandomNum 768
  let priv = PrivateKey randNum gp
  -- Generate nonce
  randBytes <- liftIO $ systemRandomBytes 256
  -- Get port number
  port <- liftIO getPortNumber
  -- Start listening on port
  socket <- liftIO $ listenOn $ PortNumber (toEnum port)
  let initialized = PeerState priv [] (-1) randBytes port Nothing socket Nothing (Left "No message entered")
  liftIO . atomically $ putTMVar state initialized
  next
peerIOInterpreter (SendOutgoing _ m next) = do
  liftIO $ sendToServer m
  next
peerIOInterpreter (GetIncoming next) = do
  ps <- readPeerState
  broadcast <- listenForBroadcast $ ps^.peerSocket
  next broadcast
peerIOInterpreter (DisplayMessage m next) = do
  liftIO $ putStrLn m
  next
peerIOInterpreter (ModifyState f next) = do
  state <- get
  liftIO . atomically $ do
    s <- takeTMVar state
    putTMVar state $ f s
  next
peerIOInterpreter (AwaitStateCondition cond next) = do
  tmvar <- get
  state <- liftIO . atomically $ do
    s <- readTMVar tmvar
    if cond s then return s else retry
  next state
peerIOInterpreter (GetUserInput next) = do
  userInput <- liftIO getLine
  next userInput
peerIOInterpreter (GetRandomInt max next) = do
  num <- liftIO $ randomNumber max
  next num
peerIOInterpreter (GetState next) = readPeerState >>= next
peerIOInterpreter (Throw err next) = throwError err >> next
peerIO :: DcPeer a -> DcPeerIO a
peerIO = iterM peerIOInterpreter

reportResult (Right a) = putStrLn "SUCCESS!"
reportResult (Left a) = print a

-- | Run our Program
runPeer = do
  peerStateTMVar <- liftIO . atomically $ newEmptyTMVar
  liftIO . forkIO . void $ runStateT (runExceptT (peerIO listenForBroadcasts)) peerStateTMVar
  (r, _) <- runStateT (runExceptT (peerIO peerProg)) peerStateTMVar
  reportResult r

listenForBroadcast :: Socket -> DcPeerIO Broadcast
listenForBroadcast s = liftIO fetch
  where fetch = do {
    (handle, addr, portNum) <- accept s;
    c <- hGetContents handle;
    case decodeBroadcast c of
      Left e -> putStrLn ("Error parsing message: " ++ e) >> fetch
      Right m -> return m
}
