module DcServerIO where

import DcServerSpec
import Messaging
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
type DcServerIO = ExceptT ServerError (StateT ServerState IO)

{-|An IO interpreter for the 'DcServer' free monad
We write the interpreter with the type:

> f (m a) -> m a

This is so we can use:

> iterM :: (Monad m, Functor f) => (f (m a) -> ma) -> Free f a -> m a

instead of writing the interpreter directly in @Free f a@ and having to type @Pure@ and @Free@ all over the place
-}
serverIO :: DcServerOperator (DcServerIO next) -> DcServerIO next
serverIO (InitServer next) = go
  where go = do {
    liftIO (putStrLn "Number of peers:");
    i <- liftIO getLine;
    case (readMaybe i :: Maybe Int) of
      Nothing -> liftIO (putStrLn "Couldn't parse number of peers, try again.") >> go
      Just num -> numPeers .= num >> next
  }
serverIO (SayString s next) = do
  liftIO $ putStrLn s
  next
serverIO (GetMessage next) = do
  ss <- get
  if ss^.numPeers == ss^.registeredPeers.to length then next $ Stream (strBytes "hey")
  else next $ PeerJoin peer
serverIO (GetServerState next) = get >>= next
serverIO (AddPeer peer next) = do
  liftIO $ putStrLn "Adding peer"
  registeredPeers %= (:) peer
  next
serverIO (AddStream s next) = do
  liftIO $ putStrLn "Adding stream"
  roundStreams %= (:) s
  next
serverIO (SendBroadcast ps msg next) = do
  liftIO $ putStrLn "Broadcasting..."
  liftIO $ mapM_ (sendToPeer msg) ps
  next
serverIO (Throw err next) = throwError err >> next

serve :: DcServer a -> DcServerIO a
serve = iterM serverIO

reportResult (Right a) = putStrLn "SUCCESS!"
reportResult (Left a) = print a

-- | Run our Program
runServer = do
  (r, _) <- runStateT (runExceptT (serve serverProg)) $ SS 0 [] []
  reportResult r
