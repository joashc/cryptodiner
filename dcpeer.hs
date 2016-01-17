{-# LANGUAGE DeriveFunctor, TemplateHaskell #-}

import Control.Monad.State
import Control.Monad.Free
import Control.Lens

data DcPeerOperator next =
    PeerConnect String next
  | GetPeers ([String] -> next)
  deriving (Functor)

data PeerState = PS {
  peers :: [String]
} deriving (Show)

data ServerState = SS {
  _numPeers :: Int,
  _registeredPeers :: [PeerState]
} deriving (Show)

makeLenses ''ServerState

data Message = PeerData | Stream deriving (Show)

data DcServerOperator next =
    InitServer next
  | GetMessage (Message -> next)
  | AddPeer PeerState next
  | SendRoundResult next
  | GetServerState (ServerState -> next)
  | SayString String next
  | Pause next
  deriving (Functor)

-- The free monad over DcServerOperator
type DcServer = Free DcServerOperator

initServer :: DcServer ()
initServer = liftF $ InitServer ()

sendResult :: DcServer ()
sendResult = liftF $ SendRoundResult ()

getMessage :: DcServer Message
getMessage = liftF $ GetMessage id

sayString :: String -> DcServer ()
sayString s = liftF $ SayString s ()

addPeer :: PeerState -> DcServer ()
addPeer p = liftF $ AddPeer p ()

getServerState :: DcServer ServerState
getServerState = liftF $ GetServerState id

pause :: DcServer ()
pause = liftF $ Pause ()

type DcServerIO = StateT ServerState IO

serverIO :: DcServerOperator (DcServerIO next) -> DcServerIO next
serverIO (InitServer next) = do
  liftIO $ putStrLn "Enter peers:"
  i <- liftIO getLine
  let num = read i :: Int
  numPeers .= num
  next
serverIO (SayString s next) = do
  liftIO $ putStrLn s
  next
serverIO (GetMessage next) = do
  ss <- get
  next PeerData
serverIO (GetServerState next) = do
  ss <- get
  next ss
serverIO (AddPeer peer next) = do
  liftIO $ putStrLn "Adding peer"
  registeredPeers %= (:) peer
  next
serverIO (Pause next) = do
  liftIO $ putStrLn "Pausing"
  x <- liftIO getLine
  next

serverProg :: DcServer ()
serverProg = do
  initServer
  forever $ getMessage >>= handler
    where handler PeerData = do
            addPeer $ PS []
            ss <- getServerState
            if ss^.numPeers <= ss^.numRegisteredPeers
            then pause
            else sayString "Waiting for more peers!"
          handler Stream = sayString "Stream"
          numRegisteredPeers = registeredPeers.to length

serve :: DcServer a -> DcServerIO a
serve = iterM serverIO

main = runStateT (serve serverProg) $ SS 0 []
