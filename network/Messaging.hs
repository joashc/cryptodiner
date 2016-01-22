{-# LANGUAGE DeriveGeneric #-}
module Messaging (Participant(..), IpAddress, ServerStatus(..), Message(..), MessageType(..), decodeMessage, send, Nonce, RoundResultData(..), RoundStream, sendToPeer, ServerMessage(..), Broadcast(..), PeerData(..), sendToServer, decodeServerMessage, decodeBroadcast) where
import Data.Serialize
import System.IO
import DiffieHellman
import qualified Data.ByteString as B
import Data.ByteString.Lazy as BL (toStrict)
import Data.ByteString.Lazy.Char8 as C (pack)
import Control.Concurrent
import Data.ByteString.Char8 as CS (unpack)
import GHC.Generics
import Network

data ServerStatus = Peering | Round
data MessageType = Ping | Peer | PeerList | ReservationStream | MessageStream | RoundResult deriving (Show, Generic, Eq)

type RoundStream = B.ByteString

type Nonce = B.ByteString

-- | The message types that may be sent to the server.
data ServerMessage = PeerJoin Participant | Stream RoundStream deriving (Show, Generic, Eq)

-- | Broadcasts are messages that are sent to all peers.
data Broadcast = PeerListB [Participant] | RoundResultB RoundStream deriving (Show, Generic, Eq)

instance Serialize ServerMessage
instance Serialize Broadcast
instance Serialize PublicKey
instance Serialize GroupParameters
instance Serialize Message
instance Serialize MessageType
instance Serialize Participant
instance Serialize RoundResultData
instance Serialize PeerData


send :: HostName -> PortNumber -> Message -> IO()
send ip portNumber m = withSocketsDo $ do
    handle <- connectTo ip $ PortNumber portNumber
    hPutStrLn handle $ CS.unpack $ encode m
    hClose handle

sendToPeer :: Broadcast -> Participant -> IO ThreadId
sendToPeer m p = forkIO $ withSocketsDo $ do
    handle <- connectTo (ipAddress p) (PortNumber (fromIntegral $ port p))
    hPutStrLn handle $ CS.unpack $ encode m
    hClose handle

sendToServer :: ServerMessage -> IO ThreadId
sendToServer m = forkIO $ withSocketsDo $ do
  handle <- connectTo "localhost" $ PortNumber 6969
  hPutStrLn handle $ CS.unpack $ encode m
  hClose handle

decodeMessage :: String -> Either String Message
decodeMessage s = decode . BL.toStrict . C.pack $ s :: Either String Message

decodeServerMessage :: String -> Either String ServerMessage
decodeServerMessage s = decode . BL.toStrict . C.pack $ s :: Either String ServerMessage

decodeBroadcast :: String -> Either String Broadcast
decodeBroadcast s = decode . BL.toStrict . C.pack $ s :: Either String Broadcast

type IpAddress = String

data PeerData = PeerData {
  publicKey :: PublicKey,
  nonce :: B.ByteString
} deriving (Generic, Show)

data Participant = Participant {
    peerPubKey :: PublicKey,
    peerNonce :: B.ByteString,
    ipAddress :: IpAddress,
    port :: Int
} deriving (Show, Generic, Eq)

data Message = Message {
    messageType :: MessageType,
    messageBody :: B.ByteString,
    portNum :: Int
} deriving (Generic, Show)

data RoundResultData = RoundResultData {
    number :: Int,
    roundData :: B.ByteString
} deriving (Generic, Show)
