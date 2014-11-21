module DcPeer where
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
    peerSocketHandler state socket
    putMVar state $ PeerState privateKey [] 0 peerPort []

peerSocketHandler :: MVar PeerState -> Socket -> IO ()
peerSocketHandler s socket = withSocketsDo $ do
    (h, addr, p) <- accept socket
    putStrLn $ show addr ++ " connected."
    contents <- hGetContents h
    let decoded = decodeMessage contents
    case decoded of
        Left err -> putStrLn err
        Right msg -> peerMessageHandler s addr p h msg
    peerSocketHandler s socket

peerMessageHandler :: MVar PeerState -> IpAddress -> PortNumber -> Handle -> Message -> IO ()
peerMessageHandler _ _ _ _ m = do
    print $ "Recieved " ++ show (messageType m) ++ " message"
    case messageType m of
        PeerList -> putStrLn "PeerList"
        _ -> putStrLn "Unknown message type"

sendKey :: MVar PeerState -> IO ()
sendKey s = withSocketsDo $ do
    state <- takeMVar s
    let publicKey = calculatePublicKey $ privKey state
    let message = Message KeyExchange (encode publicKey) $ listenPort state
    send "127.0.0.1" 6968 message
    putMVar s state

-- 

excludeSelf :: PrivateKey -> [Participant] -> [Participant]
excludeSelf priv participants = filter (\p -> peerPubKey p /= ownPubKey) participants
    where ownPubKey = calculatePublicKey priv

reservationStream :: Int -> PrivateKey -> [Participant] -> Either String B.ByteString
reservationStream res priv participants = M.join $ generateReservationStream streamLen priv pubKeys <$> resBytes
    where peers = excludeSelf priv participants
          pubKeys = map peerPubKey peers
          resBitSize = bitsForParticipants 0.01 $ length participants
          resBytes = binaryDump resBitSize res
          streamLen = resBitSize `div` 8 + 1

isReservationRound :: Int -> Int -> Bool
isReservationRound groupSize roundNum = (== 0) $ mod roundNum $ groupSize + 1
