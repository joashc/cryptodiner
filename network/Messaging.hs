{-# LANGUAGE DeriveGeneric #-}
module Messaging (Participant'(..), IpAddress, ServerStatus(..), Message(..), Header(..), MessageType(..)) where
import Data.Serialize
import DiffieHellman
import Data.ByteString.Internal as I
import GHC.Generics

data ServerStatus = Peering | RoundNegotiation | Transmitting | Closed
instance Serialize MessageType
data MessageType = Ping | KeyExchange deriving (Show, Generic, Eq)

instance Serialize PublicKey
instance Serialize GroupParameters

instance Serialize Header
data Header = Header {
    messageType :: MessageType,
    timestamp :: Integer
} deriving (Show, Generic)

type IpAddress = String

instance Serialize Participant'
data Participant' = Participant' {
    pubKey :: PublicKey,
    ipAddress :: IpAddress,
    port :: Int
} deriving (Show, Generic)

instance Serialize Message
data Message = Message {
    header :: Header,
    messageBody :: I.ByteString
} deriving (Show, Generic)
