module Reservation where
import DiffieHellman
import RandomBytes
import Messaging
import DcNetwork
import Data.List (sort, elemIndex)
import qualified Data.ByteString as B
import qualified Control.Monad as M
import Control.Applicative

bitsForParticipants :: Double -> Int -> Int
bitsForParticipants p n = round . (+ 0.5) $ -1 / (-1+(1-p)**(2/((-1+n')*n')))
    where n' = fromIntegral n :: Double

excludeSelf :: PrivateKey -> [Participant] -> [Participant]
excludeSelf priv participants = filter (\p -> peerPubKey p /= ownPubKey) participants
    where ownPubKey = calculatePublicKey priv

genReservation :: Int -> PrivateKey -> [PublicKey] -> B.ByteString -> Either String B.ByteString
genReservation byteLen privKey keys res = calculateMessageStream byteLen res =<< seeds
    where seeds = calculateSharedSeeds privKey keys

reservationStream :: Int -> PrivateKey -> [Participant] -> Int -> Either String B.ByteString
reservationStream resBitSize priv participants res = M.join $ genReservation streamLen priv pubKeys <$> resBytes
    where peers = excludeSelf priv participants
          pubKeys = map peerPubKey peers
          resBytes = binaryDump resBitSize res
          streamLen = resBitSize `div` 8 + 1

isReservationRound :: Int -> Int -> Bool
isReservationRound groupSize roundNum = (== 0) $ mod roundNum $ groupSize + 1

roundToTransmit :: Int -> [Int] -> Int -> Maybe Int
roundToTransmit current rs r = (+) nextRound <$> resIndex
    where nextRound = current + 1
          resIndex = elemIndex r $ sort rs
