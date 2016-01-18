{-# LANGUAGE DeriveFunctor, TemplateHaskell #-}
import Control.Monad.State
import Control.Monad.Free
import Control.Lens

------------------------------------------------------------
-- Define our data types
------------------------------------------------------------
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

data Message = PeerData | Stream deriving (Show)

-- Automagic some lenses with TH
makeLenses ''ServerState

------------------------------------------------------------
-- Define our DSL for the server
------------------------------------------------------------
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

------------------------------------------------------------
-- A bunch of boilerplate functions for the server operators
------------------------------------------------------------
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

------------------------------------------------------------
-- Let's get the semantics of the server written in the DSL
------------------------------------------------------------
serverProg :: DcServer ()
serverProg = do
  initServer
  forever $ getMessage >>= messageHandler

messageHandler :: Message -> DcServer ()
messageHandler PeerData = do
  addPeer $ PS []
  ss <- getServerState
  if ss^.numPeers <= ss^.registeredPeers.to length
  then pause
  else sayString "Waiting for more peers!"
messageHandler Stream = sayString "Stream"


------------------------------------------------------------
-- Let's write an interpreter that does all the nasty IO stuff
------------------------------------------------------------

-- Here's the type our serverIO interpreter is going to map the free monad to
type DcServerIO = StateT ServerState IO

{- | An IO interpreter for the DcServer free monad
  We write the interpreter with the type:
  > f (m a) -> m a
  This is so we can use:
  > iterM :: (Monad m, Functor f) => (f (m a) -> ma) -> Free f a -> m a
  instead of writing the interpreter directly in Free f a and having to type Pure
  and Free all over the place
-}
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
serverIO (GetServerState next) = get >>= next
serverIO (AddPeer peer next) = do
  liftIO $ putStrLn "Adding peer"
  registeredPeers %= (:) peer
  next
serverIO (Pause next) = do
  liftIO $ putStrLn "Pausing"
  x <- liftIO getLine
  next

-- This 
serve :: DcServer a -> DcServerIO a
serve = iterM serverIO

main = runStateT (serve serverProg) $ SS 0 []
