module DcPeerSpec where

import Data.Serialize
import Control.Lens
import Reservation
import Data.Maybe (isJust)
import Data.ByteString as B
import Control.Monad.State
import qualified Control.Monad as M (join)
import DcNetwork
import DcPeerFree
import Messaging
import DiffieHellman
import RandomBytes
import DcNodeOperator

-- | Defines the peer semantics in the 'DcPeer' DSL
peerProg :: DcPeer ()
peerProg = do
  initState
  ps <- getState
  sendOutgoing () $ PeerJoin (participantInfo ps)
  peers <- awaitPeers
  negotiateReservation
  forever $ awaitRoundResult >>= handleRoundResult

-- | Dispatch on message type
broadcastHandler :: Broadcast -> DcPeer ()
broadcastHandler (PeerListB ps) = updatePeerList ps
broadcastHandler (RoundResultB r) = updateRoundResult r

awaitRoundResult :: DcPeer RoundStream
awaitRoundResult = do
  state <- awaitStateCondition . view $ roundResult . to isJust
  modifyState $ roundResult .~ Nothing
  case state ^. roundResult of
    Nothing -> awaitRoundResult
    Just rs -> return rs

getRoundMessage :: DcPeer ()
getRoundMessage = do
  displayMessage "Enter message:"
  plaintext <- getUserInput
  modifyState $ cachedMessage .~ Right plaintext

awaitPeers :: DcPeer [Participant]
awaitPeers = do
  state <- awaitStateCondition $ (> 1) . numPeers
  return $ state ^. peers

listenForBroadcasts :: DcPeer ()
listenForBroadcasts = forever $ getIncoming >>= broadcastHandler

participantInfo :: PeerState -> Participant
participantInfo ps = Participant pub (ps^.ownNonce) "localhost" (ps^.listenPort)
  where pub = calculatePublicKey $ ps^.privateKey

updatePeerList :: [Participant] -> DcPeer ()
updatePeerList p = modifyState $ peers .~ p

updateRoundResult :: RoundStream -> DcPeer ()
updateRoundResult r = do
  state <- getState
  displayMessage $ "Round:" ++ show (state ^. roundNum)
  modifyState $ (roundResult .~ Just r) <$> (roundNum +~ 1)

handleRoundResult :: RoundStream -> DcPeer ()
handleRoundResult r = do
  displayMessage $ show (parseMessageStreams r)
  state <- getState
  if isResRoundNext state
  then negotiateReservation
  else sendNextStream

parseMessageStreams :: B.ByteString -> Either String B.ByteString
parseMessageStreams message = flip B.take messageBody <$> messageLen
    where messageBody = B.drop 8 message
          messageLen = decode . B.take 8 $ message :: Either String Int

sendNextStream :: DcPeer ()
sendNextStream = do
  stream <- getNextStream
  case stream of
    Left err -> throw $ StreamGenerationError err
    Right s -> sendOutgoing () $ Stream s

getNextStream :: DcPeer (Either String B.ByteString)
getNextStream = do
  state <- getState
  if Just (state ^. roundNum) == (state ^. reservation)
  then return . M.join $ genStream state <$> state ^. cachedMessage
  else return . M.join $ genStream state <$> Right []

sendReservation :: DcPeer ()
sendReservation = do
  state <- getState
  let resBitLen = bitsForParticipants 0.01 $ numPeers state
  num <- getRandomInt resBitLen
  let resStream = genReservationStream state resBitLen num
  case resStream of
    Left err -> throw $ ReservationError err
    Right res -> do
      modifyState $ reservation .~ Just num
      sendOutgoing () $ Stream res

negotiateReservation :: DcPeer ()
negotiateReservation = do
  getRoundMessage
  displayMessage "Negotiating reservations..."
  sendReservation
  resData <- awaitRoundResult
  state <- getState
  let transmitRound = roundToTransmit (state ^. roundNum) (toggledBitsBS resData) =<< state^.reservation
  modifyState $ reservation .~ transmitRound
  displayMessage $ "Transmitting in round: " ++ show transmitRound
  sendNextStream
