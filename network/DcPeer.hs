module DcPeer (peerMode) where
import Network
import Messaging
import Reservation
import DiffieHellman
import RandomBytes
import DcNetwork
import DhGroupParams
import Control.Concurrent
import Data.ByteString as B (ByteString, take, drop)
import Data.Serialize
import System.IO
import Control.Applicative

data PeerState = PeerState {
    privKey :: PrivateKey,
    group :: [Participant],
    roundCounter :: Int,
    reservation :: Maybe Int,
    transmitRound :: Maybe Int,
    listenPort :: Int,
    nonces :: [B.ByteString],
    cachedMessage :: Maybe String
} deriving (Show)

messageSize :: Int
messageSize = 1024

-- IO functions
peerMode :: IO ()
peerMode = do
    state <- newEmptyMVar
    randNum <- systemRandomNum 768
    let privateKey = PrivateKey randNum gp
    putStrLn "Enter port:"
    portNumber <- getLine
    let peerPort = read portNumber :: Int
    putStrLn $ "Listening on port " ++ show peerPort
    socket <- listenOn $ PortNumber (toEnum peerPort)
    putMVar state $ PeerState privateKey [] 0 Nothing Nothing peerPort [] Nothing
    nonce <- systemRandomBytes 256
    sendPeerData nonce privateKey peerPort
    peerSocketHandler state socket

peerSocketHandler :: MVar PeerState -> Socket -> IO ()
peerSocketHandler s socket = withSocketsDo $ do
    (h, addr, p) <- accept socket
    putStrLn $ show addr ++ " connected."
    _ <- forkIO $ peerConnectionHandler s addr p h
    peerSocketHandler s socket

peerConnectionHandler :: MVar PeerState -> IpAddress -> PortNumber -> Handle -> IO ()
peerConnectionHandler s ip portNumber h = do
    c <- hGetContents h
    let d = decodeMessage c
    case d of
        Right msg -> peerMessageHandler s ip portNumber h msg
        Left err -> putStrLn $ "Error receiving message: " ++ err

peerMessageHandler :: MVar PeerState -> IpAddress -> PortNumber -> Handle -> Message -> IO ()
peerMessageHandler s ip p h m = do
    print $ "Recieved " ++ show (messageType m) ++ " message"
    case messageType m of
        PeerList -> peerListHandler (decode $ messageBody m :: Either String [Participant]) s ip p
        RoundResult -> roundResultHandler ip (portNum m) (decode $ messageBody m :: Either String RoundResultData) s
        _ -> putStrLn "Unknown message type"

peerListHandler :: Either String [Participant] -> MVar PeerState -> IpAddress -> PortNumber -> IO ()
peerListHandler key s ip portNumber = do
    state <- takeMVar s
    case key of
        Right ps -> do
            forkIO $ sendReservation ip portNumber s
            putMVar s state { group = ps }
        Left err -> do
            forkIO . putStrLn $ "Could not add peers: " ++ err
            putMVar s state

sendReservation :: IpAddress -> PortNumber -> MVar PeerState -> IO ()
sendReservation ip theirPort s = do
    state <- takeMVar s
    let ps = group state
    let ourPort = listenPort state
    let priv = privKey state
    let resBitSize = bitsForParticipants 0.01 $ length ps
    res <- randomNumber resBitSize
    let roundNo = roundCounter state
    let nonceList = nonces state
    let resStream = reservationStream roundNo nonceList resBitSize priv ps res
    case resStream of
        Right stream -> do
            send "127.0.0.1" 6968 $ Message ReservationStream stream ourPort
            putMVar s state{ reservation = Just res }
        Left err -> do
            putStrLn $ "Could not send reservation: " ++ err
            putMVar s state { reservation = Nothing }

roundResultHandler :: IpAddress -> Int -> Either String RoundResultData -> MVar PeerState -> IO ()
roundResultHandler ip port msg s  = do
    state <- takeMVar s
    case msg of
        Left err -> forkIO $ putStrLn $ "Round result error: " ++ err
        Right result -> forkIO $ parseResult result $ group state
    putMVar s state
    where parseResult r g = if isReservationRound (length g) (number r) == True
            then reservationResultHandler ip port s (toggledBitsBS . xorStreams $ roundData r)
            else messageResultHandler ip port s $ roundData r

reservationResultHandler :: IpAddress -> Int -> MVar PeerState -> [Int] -> IO ()
reservationResultHandler ip port s rs = do
    state <- takeMVar s
    let current = roundCounter state
    let res = reservation state
    let round = roundToTransmit current rs =<< res
    putMVar s state{ transmitRound = round, roundCounter = current + 1 }
    sendNextMessage ip port s

messageResultHandler :: IpAddress -> Int -> MVar PeerState -> [B.ByteString] -> IO ()
messageResultHandler ip port s streams = do
    state <- takeMVar s
    print streams
    print . parseMessageStreams $ streams
    let current = roundCounter state
    putMVar s state { roundCounter = current + 1 }
    if isReservationRound (length . group $ state) (current + 1) == True
    then sendReservation ip (toEnum port :: PortNumber) s
    else sendNextMessage ip port s

parseMessageStreams :: [B.ByteString] -> Either String B.ByteString
parseMessageStreams streams = flip B.take messageBody <$> messageLen
    where message = xorStreams streams
          messageBody = B.drop 8 message
          messageLen = decode . B.take 8 $ message :: Either String Int

sendNextMessage :: IpAddress -> Int -> MVar PeerState -> IO ()
sendNextMessage ip port s = do
    state <- takeMVar s
    let roundNo = roundCounter state
    let nonceList = nonces state
    if Just (roundCounter state) == transmitRound state
    then do
        putStrLn "Enter message:"
        message <- getLine
        let stream = generateStream roundNo nonceList messageSize message (privKey state) (map peerPubKey . group $ state)
        sendStream (listenPort state) stream ip port
    else do
        let stream = generateStream roundNo nonceList messageSize [] (privKey state) (map peerPubKey . group $ state)
        sendStream (listenPort state) stream ip port
    putMVar s state

sendStream :: Int -> Either String B.ByteString -> IpAddress -> Int -> IO ()
sendStream ownPort stream theirIp theirPort = 
    case stream of
        Left err -> putStrLn $ "Error sending stream: " ++ err
        Right s -> send theirIp (toEnum theirPort :: PortNumber) $ Message MessageStream s ownPort

sendPeerData :: B.ByteString -> PrivateKey -> Int -> IO ()
sendPeerData nonce privKey port = withSocketsDo $ do
    putStrLn "Sending public key"
    let publicKey = calculatePublicKey privKey
    let message = Message Peer (encode $ PeerData publicKey nonce) port
    send "127.0.0.1" 6968 message
