import Network
import Reservation
import Control.Applicative
import qualified Control.Monad as M (join)
import Messaging
import DcPeer
import RandomBytes
import Data.Serialize
import System.IO
import System.Environment
import Control.Concurrent
import DhGroupParams
import DcNetwork
import DiffieHellman
import qualified Data.ByteString as B (ByteString)

data ServerState = ServerState {
    peers :: [Participant],
    status :: ServerStatus,
    groupSize :: Int,
    roundStreams :: [B.ByteString],
    listenPort :: Int,
    privKey :: PrivateKey,
    roundNo :: Int
}

roundBytes :: Int
roundBytes = 255

main :: IO ()
main = getArgs >>= parseArgs

parseArgs :: [String] -> IO ()
parseArgs ["-p"] = peerMode
parseArgs _ = serverMode

serverMode :: IO ()
serverMode = withSocketsDo $ do
    putStrLn "Participants: "
    grpSize <- getLine
    state <- newEmptyMVar
    p <- systemRandomNum 768
    putStrLn $ "Using private key: " ++ show p
    let privateKey = PrivateKey p gp
    putMVar state $ ServerState [] Peering (read grpSize :: Int) [] 6968 privateKey 0
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
        Peer -> keyExchangeHandler (decode $ messageBody m :: Either String PeerData) state ip (portNum m) h
        ReservationStream -> do
            streamReciever (messageBody m) state
        MessageStream -> do
            streamReciever (messageBody m) state
        _ -> putStrLn "Unknown message type"

keyExchangeHandler :: Either String PeerData -> MVar ServerState -> IpAddress -> Int -> Handle -> IO ()
keyExchangeHandler p state ip portNumber handle =
    case p of
        Right peerData -> do
                     s <- takeMVar state
                     let peer = Participant (publicKey peerData) (nonce peerData) ip portNumber
                     let newPs = peer:(peers s)
                     if length newPs == groupSize s
                        then forkIO $ sendPeerList state
                        else forkIO $ putStrLn "Waiting for peers"
                     putMVar state s{ peers = newPs }
        Left e -> putStrLn $ "Could not parse public key: " ++ e

streamHandler :: Either String B.ByteString -> MVar ServerState -> IO ()
streamHandler (Left e) _ = putStrLn $ "Error parsing stream: " ++ e
streamHandler (Right stream) state = streamReciever stream state

streamReciever :: B.ByteString -> MVar ServerState -> IO ()
streamReciever stream s = do
    state <- takeMVar s
    let streams = stream:roundStreams state
    if length streams == groupSize state
        then do
            let round = roundNo state
            broadcastRoundResult (listenPort state) round (peers state) streams
            putMVar s state{ roundStreams = [], roundNo = round + 1 }
        else do
            putStrLn "Waiting for the rest of the peers"
            putMVar s state{ roundStreams = streams }

broadcastRoundResult :: Int -> Int -> [Participant] -> [B.ByteString] -> IO ()
broadcastRoundResult portNo roundNum ps streams = withSocketsDo $ do
    let roundResultMsg = Message RoundResult (encode $ RoundResultData roundNum streams) portNo
    sendToAllPeers roundResultMsg ps

sendPeerList :: MVar ServerState -> IO ()
sendPeerList s = withSocketsDo $ do
    state <- takeMVar s
    let ps = peers state
    let peerListMsg = Message PeerList (encode $ peers state) $ listenPort state
    sendToAllPeers peerListMsg ps
    putMVar s state

sendToAllPeers :: Message -> [Participant] -> IO ()
sendToAllPeers msg = mapM_ (\p -> forkIO $ send (ipAddress p) (toEnum $ port p :: PortNumber) msg)
