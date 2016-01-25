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
import DcNodeOperator

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

{-|An IO interpreter for the 'DcServer' free monad
We write the interpreter with the type:

> f (m a) -> m a

This is so we can use:

> iterM :: (Monad m, Functor f) => (f (m a) -> ma) -> Free f a -> m a

instead of writing the interpreter directly in @Free f a@ and having to type @Pure@ and @Free@ all over the place
-}
serverIO :: DcServerOperator (DcServerIO next) -> DcServerIO next
serverIO (InitState next) = do
  n <- liftIO getGroupSize
  socket <- liftIO $ listenOn $ PortNumber 6969
  let initialized = SS n [] [] (Just socket)
  state <- get
  liftIO . atomically $ putTMVar state initialized
  next
serverIO (DisplayMessage s next) = do
  liftIO $ putStrLn s
  next
serverIO (GetIncoming next) = do
  ss <- readState
  msg <- listenForMessage $ ss^.listenSocket
  next msg
serverIO (GetState next) = readState >>= next
serverIO (GetUserInput next) = liftIO getLine >>= next
serverIO (ModifyState f next) = do
  state <- get
  liftIO . atomically $ do
    s <- takeTMVar state
    putTMVar state $ f s
  next
serverIO (SendOutgoing ps msg next) = do
  liftIO $ putStrLn "Broadcasting..."
  liftIO $ mapM_ (sendToPeer msg) ps
  next
serverIO (AwaitStateCondition cond next) = do
  tmvar <- get
  state <- liftIO . atomically $ do
    s <- readTMVar tmvar
    if cond s then return s else retry
  next state
serverIO (Throw err next) = throwError err >> next


listenForMessage :: Maybe Socket -> DcServerIO ServerMessage
listenForMessage Nothing = throwError SocketError
listenForMessage (Just s) = liftIO fetch
  where
    fetch = do
      (handle, addr, portNum) <- accept s
      putStrLn $ show addr ++ " : " ++ show portNum  ++ " connected."
      c <- hGetContents handle;
      case decodeServerMessage c of
        Left e -> putStrLn ("Error parsing message: " ++ e) >> fetch
        Right m -> return m

serve :: DcServer a -> DcServerIO a
serve = iterM serverIO

reportResult (Right a) = putStrLn "SUCCESS!"
reportResult (Left a) = print a

runWithState :: TMVar ServerState -> DcServer a -> IO (Either ServerError a, TMVar ServerState)
runWithState state prog = runStateT (runExceptT (serve prog)) state

-- | Starts the message listening loop and the main server thread
runServer :: IO ()
runServer = do
  serverStateTMVar <- liftIO . atomically $ newEmptyTMVar
  liftIO . forkIO . void $ runWithState serverStateTMVar listenForMessages
  (r, _) <- runWithState serverStateTMVar serverProg
  reportResult r
