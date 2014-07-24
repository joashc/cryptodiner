{-# LANGUAGE DeriveGeneric #-}
import Network
import Messaging
import Data.Serialize
import System.IO
import Data.ByteString.Internal as I
import Data.ByteString.Lazy as BL (toStrict)
import Data.ByteString.Lazy.Char8 as C (pack)
import Control.Concurrent
import DcNetworkExample
import DiffieHellman

data ServerState = ServerState {
    peers :: [Participant'],
    status :: ServerStatus
}

privateKey = PrivateKey 54238578399943587349 gp

createParticipant :: Integer -> IpAddress -> PortNumber -> Participant'
createParticipant e ip port = Participant' pubKey ip $ fromIntegral port
    where pubKey = PublicKey e gp

main :: IO ()
main = withSocketsDo $ do
    state <- newEmptyMVar
    putMVar state $ ServerState [] Peering
    let port = 6968
    putStrLn $ "Listening on port " ++ show port
    socket <- listenOn $ PortNumber port
    socketHandler state socket

socketHandler :: MVar ServerState -> Socket -> IO ()
socketHandler state s = do
    (handle, addr, port) <- accept s
    putStrLn $ show addr ++ " connected."
    forkIO $ connectionHandler state addr port handle 
    socketHandler state s

connectionHandler :: MVar ServerState -> IpAddress -> PortNumber -> Handle -> IO ()
connectionHandler state ip port h = do
    e <- hGetContents h
    let msg = decode . BL.toStrict . C.pack $ e :: Either String Message
    hPutStrLn h $ "Recieved Public Key: " ++ e
    case messageType . header $ Right msg of
        KeyExchange -> keyExchangeHandler (messageBody e) state ip port h
    hClose h

keyExchangeHandler :: I.ByteString -> MVar ServerState -> IpAddress -> PortNumber -> Handle -> IO ()
keyExchangeHandler b state ip port handle = do
    let body = decode b
    let peer = decode $ body :: Maybe PublicKey
    s <- takeMVar state
    let newPs = peer:(peers s)
    putMVar state s{peers = newPs}
