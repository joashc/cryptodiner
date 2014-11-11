{-# LANGUAGE DeriveGeneric #-}
import Network
import Messaging
import Data.Serialize
import System.IO
import System.Environment
import Control.Concurrent
import DcNetworkExample
import DcNetwork
import DiffieHellman
import qualified Data.ByteString as B (ByteString)

data ServerState = ServerState {
    peers :: [Participant'],
    status :: ServerStatus,
    groupSize :: Int,
    roundStreams :: [B.ByteString],
    listenPort :: Int,
    privKey :: PrivateKey
}

serverPrivateKey = PrivateKey 54238578399943587349 gp

createParticipant :: Integer -> IpAddress -> PortNumber -> Participant'
createParticipant e ip port = Participant' pubKey ip $ fromIntegral port
    where pubKey = PublicKey e gp

main :: IO ()
main = getArgs >>= parseArgs

parseArgs :: [String] -> IO ()
parseArgs ["-p"] = peerMode
parseArgs _ = serverMode

peerMode :: IO ()
peerMode = withSocketsDo $ do
    state <- newEmptyMVar
    putStrLn "Enter private key:"
    p <- getLine
    let privKey = PrivateKey (read p :: Integer) gp
    let pubKey = calculatePublicKey privKey
    putStrLn "Enter port:"
    portNum <- getLine
    let port = read portNum :: Int
    putMVar state $ ServerState [] Peering 0 [] port privKey
    let message = Message KeyExchange (encode $ pubKey) port
    send "127.0.0.1" 6968 message
    putStrLn $ "Listening on port " ++ show port
    socket <- listenOn $ PortNumber (toEnum port)
    peerSocketHandler state socket

peerSocketHandler :: MVar ServerState -> Socket -> IO ()
peerSocketHandler state s = withSocketsDo $ do
    (h, addr, port) <- accept s
    putStrLn $ show addr ++ " connected."
    m <- hGetContents h
    let msg = decodeMessage m
    case msg of
        Right m -> messageHandler m state addr port h
        Left e -> print $ "Error parsing message: " ++ e
    peerSocketHandler state s

serverMode :: IO ()
serverMode = withSocketsDo $ do
    putStrLn $ "Participants: "
    groupSize <- getLine
    state <- newEmptyMVar
    putMVar state $ ServerState [] Peering (read groupSize :: Int) [] 6968 serverPrivateKey
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
    let msg = decodeMessage e
    case msg of
        Right m -> messageHandler m state ip port h
        Left e -> print $ "Could not parse message: " ++ e

messageHandler :: Message -> MVar ServerState -> IpAddress -> PortNumber -> Handle -> IO ()
messageHandler m state ip port h = do
    print $ "Recieved " ++ (show $ messageType m) ++ " message"
    case messageType m of
        KeyExchange -> keyExchangeHandler (decode $ messageBody m :: Either String PublicKey) state ip (portNum m) h
        PeerList -> peerListHandler (decode $ messageBody m :: Either String [Participant']) state
        RequestStream -> requestTransmissionHandler state ip (toEnum $ portNum m :: PortNumber)
        Stream -> streamHandler (decode $ messageBody m :: Either String B.ByteString) state

appendParticipants :: [Participant'] -> MVar ServerState -> IO ()
appendParticipants ps state = do
        s <- takeMVar state
        let newPs = ps ++ peers s
        putMVar state s{peers = newPs}
        case length newPs == groupSize s of
            True -> sendPeerList state
            False -> putStrLn "Waiting for more peers"

keyExchangeHandler :: Either String PublicKey -> MVar ServerState -> IpAddress -> Int -> Handle -> IO ()
keyExchangeHandler key state ip port handle = do
    case key of
        Right p -> do
                     let peer = [Participant' p ip port]
                     appendParticipants peer state
        Left e -> do
                     hPutStrLn handle $ "Could not parse public key: " ++ e

peerListHandler :: Either String [Participant'] -> MVar ServerState -> IO ()
peerListHandler (Right ps) state = do
        s <- takeMVar state
        forkIO $ appendParticipants [p | p <- ps, (port' p) /= (listenPort s)] state
        putMVar state s
peerListHandler (Left e) state = putStrLn $ "Could not add participants: " ++ e

requestTransmissionHandler :: MVar ServerState -> IpAddress -> PortNumber -> IO ()
requestTransmissionHandler s ip port = withSocketsDo $ do
    putStrLn "Enter message:"
    message <- getLine
    state <- takeMVar s
    let stream = generateSingleStream message (privKey state) (map pubKey' . peers $ state)
    case stream of
        Left e -> putStrLn "Error generating stream"
        Right s -> send ip port $ Message Stream (encode s) (listenPort state)
    putMVar s state

streamHandler :: Either String B.ByteString -> MVar ServerState -> IO ()
streamHandler (Left e) _ = putStrLn "Error parsing stream"
streamHandler (Right stream) state = streamReciever stream state

streamReciever :: B.ByteString -> MVar ServerState -> IO ()
streamReciever stream s = do
    state <- takeMVar s
    let streams = stream:(roundStreams state)
    putMVar s state{ roundStreams = streams }
    case (length streams == groupSize state) of
        True -> broadcastRoundResult s
        False -> putStrLn "Waiting for the rest of the peers"

broadcastRoundResult :: MVar ServerState -> IO ()
broadcastRoundResult s = withSocketsDo $ do
    state <- takeMVar s
    let streams = roundStreams state
    print streams
    print $ xorStreams streams
    putMVar s state{ roundStreams=[], status = Transmitting }

sendPeerList :: MVar ServerState -> IO ()
sendPeerList s = withSocketsDo $ do
    state <- takeMVar s
    let ps = peers state
    mapM_ (\p -> forkIO $ send (ipAddress p) (toEnum $ port' p :: PortNumber) $ Message PeerList (encode $ peers state) $ listenPort state) ps
    print $ listenPort state
    mapM_ (\p -> forkIO $ send (ipAddress p) (toEnum $ port' p :: PortNumber) $ Message RequestStream (encode $ peers state) $ listenPort state) ps
    putMVar s state{status=Transmitting}
