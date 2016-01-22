module DcServerIO where

import DcServerSpec
import System.IO
import Messaging
import Network
import Control.Concurrent
import Control.Concurrent.STM
import DiffieHellman
import DcServerFree
import Control.Lens
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Free
import Text.Read (readMaybe)
import RandomBytes (strBytes)

pk = PublicKey 589430589430 (GroupParameters 5834759 753489573849)

peer = Participant pk (strBytes "5353") "localhost" 5435

-- Here's the type our serverIO interpreter is going to map the free monad to
type DcServerIO = ExceptT ServerError (StateT (TMVar ServerState) IO)

getGroupSize :: IO Int
getGroupSize = go
  where
    go = do
      putStrLn "Number of peers:"
      i <- getLine
      case (readMaybe i :: Maybe Int) of
        Nothing -> putStrLn "Couldn't parse number of peers, try again." >> go
        Just num -> return num


-- | Reads state from TMVar. This does not synchronise with other threads, use with caution.
readState :: DcServerIO ServerState
readState = do
  state <- get
  liftIO . atomically $ readTMVar state

-- | Swaps state with a new state.
swapState :: ServerState -> DcServerIO ServerState
swapState s = do
  state <- get
  liftIO . atomically $ swapTMVar state s

-- | Atomically modify state. Will block if TMVar is empty.
modifyStateAtomically :: (ServerState -> ServerState) -> DcServerIO ()
modifyStateAtomically f = do
  state <- get
  liftIO . atomically $ do
    s <- takeTMVar state
    putTMVar state $ f s

{-|An IO interpreter for the 'DcServer' free monad
We write the interpreter with the type:

> f (m a) -> m a

This is so we can use:

> iterM :: (Monad m, Functor f) => (f (m a) -> ma) -> Free f a -> m a

instead of writing the interpreter directly in @Free f a@ and having to type @Pure@ and @Free@ all over the place
-}
serverIO :: DcServerOperator (DcServerIO next) -> DcServerIO next
serverIO (InitServer next) = do
  n <- liftIO getGroupSize
  ss <- readState
  socket <- liftIO $ listenOn $ PortNumber 6969
  let initialized = ss & (numPeers .~ n) <$> (listenSocket .~ Just socket)
  _ <- swapState initialized
  next
serverIO (SayString s next) = do
  liftIO $ putStrLn s
  next
serverIO (GetMessage next) = do
  ss <- readState
  msg <- listenForMessage $ ss^.listenSocket
  next msg
serverIO (GetServerState next) = readState >>= next
serverIO (AddPeer peer next) = do
  liftIO $ putStrLn "Adding peer"
  _ <- modifyStateAtomically $ addPeerIfNeeded peer
  next
serverIO (AddStream s next) = do
  liftIO $ putStrLn "Adding stream"
  _ <- modifyStateAtomically $ addStreamIfNeeded s
  next
serverIO (SendBroadcast ps msg next) = do
  liftIO $ putStrLn "Broadcasting..."
  liftIO $ mapM_ (sendToPeer msg) ps
  next
serverIO (Throw err next) = throwError err >> next

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

listenForMessage :: Maybe Socket -> DcServerIO ServerMessage
listenForMessage Nothing = throwError SocketError
listenForMessage (Just s) = liftIO fetch
  where
    fetch = do
      (handle, addr, portNum) <- accept s
      putStrLn $ show addr ++ show portNum  ++ " connected."
      c <- hGetContents handle;
      case decodeServerMessage c of
        Left e -> putStrLn ("Error parsing message: " ++ e) >> fetch
        Right m -> return m

serve :: DcServer a -> DcServerIO a
serve = iterM serverIO

reportResult (Right a) = putStrLn "SUCCESS!"
reportResult (Left a) = print a

-- | Run our Program
runServer = do
  serverStateTMVar <- liftIO . atomically $ newTMVar $ SS 0 [] [] Nothing
  (r, _) <- runStateT (runExceptT (serve serverProg)) serverStateTMVar
  reportResult r
