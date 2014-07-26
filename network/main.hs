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
    e <- hGetLine h
    let msg = decode . BL.toStrict . C.pack $ e :: Either String Message
    case msg of
        Right m -> messageHandler m state ip port h
        Left e -> do
            hPutStrLn h $ "Could not parse message: " ++ e
    hClose h

messageHandler :: Message -> MVar ServerState -> IpAddress -> PortNumber -> Handle -> IO ()
messageHandler m state ip port h = case messageType . header $ m of
    KeyExchange -> keyExchangeHandler (decode $ messageBody m :: Either String PublicKey) state ip port h

keyExchangeHandler :: Either String PublicKey -> MVar ServerState -> IpAddress -> PortNumber -> Handle -> IO ()
keyExchangeHandler key state ip port handle = do
    case key of
        Right p -> do
                     let peer = Participant' p ip (fromIntegral port :: Int)
                     s <- takeMVar state
                     print peer
                     let newPs = peer:(peers s)
                     putMVar state s{peers = newPs}
        Left e -> do
                     hPutStrLn handle $ "Could not parse public key: " ++ e
