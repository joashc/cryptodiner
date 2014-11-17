import Network
import Messaging
import RandomBytes
import Data.Serialize
import System.IO
import System.Environment
import Control.Concurrent
import DcNetworkExample
import DcNetwork
import DiffieHellman
import qualified Data.ByteString as B (ByteString)

data ServerState = ServerState {
    peers :: [Participant],
    status :: ServerStatus,
    groupSize :: Int,
    roundStreams :: [B.ByteString],
    listenPort :: Int,
    privKey :: PrivateKey
}

roundBytes :: Int
roundBytes = 255

main :: IO ()
main = getArgs >>= parseArgs

parseArgs :: [String] -> IO ()
parseArgs ["-p"] = peerMode
parseArgs _ = serverMode

peerMode :: IO ()
peerMode = withSocketsDo $ do
    state <- newEmptyMVar
    p <- systemRandomBytes 768
    putStrLn $ "Using private key: " ++ show p
    let peerPrivateKey = PrivateKey p gp
    let peerPublicKey = calculatePublicKey peerPrivateKey
    putStrLn "Enter port:"
    portNumber <- getLine
    let peerPort = read portNumber :: Int
    putMVar state $ ServerState [] Peering 0 [] peerPort peerPrivateKey
    let message = Message KeyExchange (encode peerPublicKey) peerPort
    send "127.0.0.1" 6968 message
    putStrLn $ "Listening on port " ++ show peerPort
    socket <- listenOn $ PortNumber (toEnum peerPort)
    peerSocketHandler state socket

peerSocketHandler :: MVar ServerState -> Socket -> IO ()
peerSocketHandler state s = withSocketsDo $ do
    (h, addr, p) <- accept s
    putStrLn $ show addr ++ " connected."
    c <- hGetContents h
    let msg = decodeMessage c
    case msg of
        Right m -> messageHandler m state addr p h
        Left e -> print $ "Error parsing message: " ++ e
    peerSocketHandler state s

serverMode :: IO ()
serverMode = withSocketsDo $ do
    putStrLn "Participants: "
    grpSize <- getLine
    state <- newEmptyMVar
    p <- systemRandomBytes 768
    putStrLn $ "Using private key: " ++ show p
    let privateKey = PrivateKey p gp
    putMVar state $ ServerState [] Peering (read grpSize :: Int) [] 6968 privateKey
    let portNumber = 6968
    putStrLn $ "Listening on port " ++ show portNumber
    socket <- listenOn $ PortNumber portNumber
    socketHandler state socket

socketHandler :: MVar ServerState -> Socket -> IO ()
socketHandler state s = do
    (handle, addr, portNumber) <- accept s
    putStrLn $ show addr ++ " connected."
    _ <- forkIO $ connectionHandler state addr portNumber handle
    socketHandler state s

connectionHandler :: MVar ServerState -> IpAddress -> PortNumber -> Handle -> IO ()
connectionHandler state ip portNumber h = do
    c <- hGetContents h
    let msg = decodeMessage c
    case msg of
        Right m -> messageHandler m state ip portNumber h
        Left e -> print $ "Could not parse message: " ++ e

messageHandler :: Message -> MVar ServerState -> IpAddress -> PortNumber -> Handle -> IO ()
messageHandler m state ip _ h = do
    print $ "Recieved " ++ show (messageType m) ++ " message"
    case messageType m of
        KeyExchange -> keyExchangeHandler (decode $ messageBody m :: Either String PublicKey) state ip (portNum m) h
        PeerList -> peerListHandler (decode $ messageBody m :: Either String [Participant]) state
        RequestStream -> requestTransmissionHandler state ip (toEnum $ portNum m :: PortNumber)
        Stream -> streamHandler (decode $ messageBody m :: Either String B.ByteString) state
        _ -> putStrLn "Unknown message type"
        --Reservation -> reservationHandler (decode $ messageBody m :: Either String Integer) state
        --RequestReservation -> reservationHandler (decode $ messageBody m :: Either String Integer) state

appendParticipants :: [Participant] -> MVar ServerState -> IO ()
appendParticipants ps state = do
        s <- takeMVar state
        let newPs = ps ++ peers s
        putMVar state s{peers = newPs}
        if length newPs == groupSize s
            then sendPeerList state
            else putStrLn "Waiting for more peers"

keyExchangeHandler :: Either String PublicKey -> MVar ServerState -> IpAddress -> Int -> Handle -> IO ()
keyExchangeHandler key state ip portNumber handle =
    case key of
        Right p -> do
                     let peer = [Participant p ip portNumber]
                     appendParticipants peer state
        Left e -> hPutStrLn handle $ "Could not parse public key: " ++ e

peerListHandler :: Either String [Participant] -> MVar ServerState -> IO ()
peerListHandler (Right ps) state = do
        s <- takeMVar state
        _ <- forkIO $ appendParticipants [p | p <- ps, port p /= listenPort s] state
        putMVar state s
peerListHandler (Left e) _ = putStrLn $ "Could not add participants: " ++ e

requestTransmissionHandler :: MVar ServerState -> IpAddress -> PortNumber -> IO ()
requestTransmissionHandler s ip portNumber = withSocketsDo $ do
    putStrLn "Enter message:"
    message <- getLine
    state <- takeMVar s
    let stream = generateMessageStream roundBytes message (privKey state) (map publicKey . peers $ state)
    case stream of
        Left e -> putStrLn $ "Error generating stream: " ++ show e
        Right msg -> send ip portNumber $ Message Stream (encode msg) (listenPort state)
    putMVar s state

streamHandler :: Either String B.ByteString -> MVar ServerState -> IO ()
streamHandler (Left e) _ = putStrLn $ "Error parsing stream: " ++ e
streamHandler (Right stream) state = streamReciever stream state

streamReciever :: B.ByteString -> MVar ServerState -> IO ()
streamReciever stream s = do
    state <- takeMVar s
    let streams = stream:roundStreams state
    putMVar s state{ roundStreams = streams }
    if length streams == groupSize state
        then broadcastRoundResult s
        else putStrLn "Waiting for the rest of the peers"

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
    mapM_ (\p -> forkIO $ send (ipAddress p) (toEnum $ port p :: PortNumber) $ Message PeerList (encode $ peers state) $ listenPort state) ps
    print $ listenPort state
    mapM_ (\p -> forkIO $ send (ipAddress p) (toEnum $ port p :: PortNumber) $ Message RequestReservation (encode $ peers state) $ listenPort state) ps
    mapM_ (\p -> forkIO $ send (ipAddress p) (toEnum $ port p :: PortNumber) $ Message RequestStream (encode $ peers state) $ listenPort state) ps
    putMVar s state{status=Transmitting}
