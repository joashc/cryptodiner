module DcPeer (peerMode) where
import Network
import Messaging
import Reservation
import DiffieHellman
import RandomBytes
import DcNetwork
import DhGroupParams
import Control.Concurrent
import Data.ByteString as B (ByteString)
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
    nonces :: [B.ByteString]
} deriving (Show)

-- IO functions
peerMode :: IO ()
peerMode = do
    state <- newEmptyMVar
    randNum <- systemRandomBytes 768
    let privateKey = PrivateKey randNum gp
    putStrLn "Enter port:"
    portNumber <- getLine
    let peerPort = read portNumber :: Int
    putStrLn $ "Listening on port " ++ show peerPort
    socket <- listenOn $ PortNumber (toEnum peerPort)
    putMVar state $ PeerState privateKey [] 0 Nothing Nothing peerPort []
    sendPeerData privateKey peerPort
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
            let resBitSize = bitsForParticipants 0.01 $ length ps
            res <- randomNumber resBitSize
            let resStream = reservationStream resBitSize (privKey state) ps res
            case resStream of
                Right stream -> do
                    send ip 6968 $ Message ReservationStream stream (listenPort state)
                    putMVar s state{ group = ps, reservation = Just res }
                Left err -> do
                    putStrLn $ "Could not send reservation: " ++ err
                    putMVar s state { group = ps }
        Left err -> do
            putStrLn $ "Could not add peers: " ++ err
            putMVar s state

roundResultHandler :: IpAddress -> Int -> Either String RoundResultData -> MVar PeerState -> IO ()
roundResultHandler ip port msg s  = do
    state <- takeMVar s
    case msg of
        Left err -> forkIO $ putStrLn $ "Round result error: " ++ err
        Right result -> forkIO $ parseResult result $ group state
    putMVar s state
    sendNextMessage ip port s
    where parseResult r g = if isReservationRound (length g) (number r) == True
            then reservationResultHandler s (toggledBitsBS . xorStreams $ roundData r)
            else messageResultHandler s $ roundData r

reservationResultHandler :: MVar PeerState -> [Int] -> IO ()
reservationResultHandler s rs = do
    state <- takeMVar s
    let current = roundCounter state
    let res = reservation state
    let round = roundToTransmit current rs =<< res
    print round
    putMVar s state{ transmitRound = round, roundCounter = current + 1 }

messageResultHandler :: MVar PeerState -> [B.ByteString] -> IO ()
messageResultHandler s streams = do
    state <- takeMVar s
    print . xorStreams $ streams
    let current = roundCounter state
    putMVar s state { roundCounter = current + 1 }

sendNextMessage :: IpAddress -> Int -> MVar PeerState -> IO ()
sendNextMessage ip port s = do
    state <- takeMVar s
    if Just (roundCounter state) == transmitRound state
    then do
        message <- getLine
        let stream = generateStream 255 message (privKey state) (map peerPubKey . group $ state)
        sendStream (listenPort state) stream ip port
    else do
        let stream = generateStream 255 [] (privKey state) (map peerPubKey . group $ state)
        sendStream (listenPort state) stream ip port

sendStream :: Int -> Either String B.ByteString -> IpAddress -> Int -> IO ()
sendStream ownPort stream theirIp theirPort = 
    case stream of
        Left err -> putStrLn $ "Error sending stream: " ++ err
        Right s -> send theirIp (toEnum theirPort :: PortNumber) $ Message MessageStream s ownPort

sendPeerData :: PrivateKey -> Int -> IO ()
sendPeerData privKey port = withSocketsDo $ do
    putStrLn "Sending public key"
    let publicKey = calculatePublicKey privKey
    let message = Message Peer (encode $ PeerData publicKey (strBytes "hello")) port
    send "127.0.0.1" 6968 message
