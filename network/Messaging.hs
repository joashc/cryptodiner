{-# LANGUAGE DeriveGeneric #-}
module Messaging (Participant'(..), IpAddress, ServerStatus(..), Message(..), MessageType(..), decodeMessage, send) where
import Data.Serialize
import System.IO
import DiffieHellman
import Data.ByteString.Internal as I
import Data.ByteString.Lazy as BL (toStrict)
import qualified Data.ByteString.Lazy.Char8 as C (pack, unpack, hPutStrLn)
import qualified Data.ByteString.Char8 as CS (unpack)
import GHC.Generics
import Network

data ServerStatus = Peering | RoundNegotiation | Transmitting | Closed
data MessageType = Ping | KeyExchange | PeerList | RequestStream | Stream | CombinedStream deriving (Show, Generic, Eq)

instance Serialize PublicKey
instance Serialize GroupParameters
instance Serialize Message
instance Serialize MessageType
instance Serialize Participant'

send :: HostName -> PortNumber -> Message -> IO()
send ip port m = withSocketsDo $ do
    handle <- connectTo ip $ PortNumber port
    hPutStrLn handle $ CS.unpack $ encode m

decodeMessage :: String -> Either String Message
decodeMessage s = decode . BL.toStrict . C.pack $ s :: Either String Message

type IpAddress = String

data Participant' = Participant' {
    pubKey' :: PublicKey,
    ipAddress :: IpAddress,
    port' :: Int
} deriving (Show, Generic)

data Message = Message {
    messageType :: MessageType,
    messageBody :: I.ByteString,
    portNum :: Int
} deriving (Generic, Show)

