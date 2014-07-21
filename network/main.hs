{-# LANGUAGE DeriveGeneric #-}
import Network
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import System.IO
import Control.Concurrent
import DcNetworkExample
import DiffieHellman
import qualified Data.ByteString.Lazy as BL (toChunks, ByteString)
import qualified Data.ByteString as B (concat, ByteString)
import qualified Data.ByteString.Char8 as C (hPutStrLn)
import GHC.Generics

privateKey = PrivateKey 54238578399943587349 gp

data Participant' = Participant' { 
    pubKey :: PublicKey,
    ipAddress :: IpAddress,
    port :: Int
} deriving (Show, Generic)

instance FromJSON Participant'
instance ToJSON Participant'
instance FromJSON GroupParameters
instance ToJSON GroupParameters
instance FromJSON PublicKey
instance ToJSON PublicKey

data ServerStatus = Peering | RoundNegotiation | Transmitting | Closed
type IpAddress = String

main :: IO ()
main = withSocketsDo $ do
    state <- newEmptyMVar
    putMVar state []
    let port = 6968
    putStrLn $ "Listening on port " ++ show port
    socket <- listenOn $ PortNumber port
    socketHandler state socket

socketHandler :: MVar [Participant'] -> Socket -> IO ()
socketHandler state s = do
    (handle, addr, port) <- accept s
    putStrLn $ show addr ++ " connected."
    forkIO $ connectionHandler state addr port handle 
    socketHandler state s

connectionHandler :: MVar [Participant'] -> IpAddress -> PortNumber -> Handle -> IO ()
connectionHandler state ip port h = do
    e <- hGetLine h
    hPutStrLn h $ "Recieved Public Key: " ++ e
    let exp = fromIntegral (read $ e :: Integer)
    let newParticipant = createParticipant exp ip port
    ps <- takeMVar state
    let newPs = newParticipant:ps
    putMVar state newPs
    C.hPutStrLn h (toStrict $ encode newPs)
    hClose h

createParticipant :: Integer -> IpAddress -> PortNumber -> Participant'
createParticipant e ip port = Participant' pubKey ip $ fromIntegral port
    where pubKey = PublicKey e gp

toStrict :: BL.ByteString -> B.ByteString
toStrict = B.concat . BL.toChunks
