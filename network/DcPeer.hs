module DcPeer (peerMode) where
import Network
import Messaging
import Reservation
import DiffieHellman
import RandomBytes
import DcNetwork
import DhGroupParams
import Control.Concurrent
import qualified Control.Monad as M (join)
import Data.ByteString as B (ByteString)
import Data.Serialize
import System.IO
import Control.Applicative

data PeerState = PeerState {
    privKey :: PrivateKey,
    group :: [Participant],
    roundCounter :: Int,
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
    putMVar state $ PeerState privateKey [] 0 peerPort []
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
        _ -> putStrLn "Unknown message type"

peerListHandler :: Either String [Participant] -> MVar PeerState -> IpAddress -> PortNumber -> IO ()
peerListHandler key s ip portNumber = do
    state <- takeMVar s
    case key of
        Right ps -> do
            res <- genReservationIO (privKey state) ps
            case res of
                Right stream -> send "127.0.0.1" 6968 $ Message ReservationStream stream (listenPort state)
                Left err -> putStrLn $ "Could not send reservation: " ++ err
            putMVar s state{ group = ps }
        Left err -> putStrLn $ "Could not add peers: " ++ err

--roundResultHandler :: Either String RoundResult -> 

genReservationIO :: PrivateKey -> [Participant] -> IO (Either String ByteString)
genReservationIO pk ps = reservationStream resBitSize pk ps <$> res
    where resBitSize = bitsForParticipants 0.01 $ length ps
          res = randomNumber resBitSize

sendPeerData :: PrivateKey -> Int -> IO ()
sendPeerData privKey port = withSocketsDo $ do
    putStrLn "Sending public key"
    let publicKey = calculatePublicKey privKey
    let message = Message Peer (encode $ PeerData publicKey (strBytes "hello")) port
    send "127.0.0.1" 6968 message
