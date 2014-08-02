module DcNetwork (Participant(..), keyExchange, generateStream, generateStreams, combineStreams, generateSingleStream, xorStreams) where
import DiffieHellman
import Crypto.Random.DRBG
import Data.List (nub)
import Data.Either
import Data.ByteString as B (ByteString)
import Data.Bits
import RandomBytes

-- Bytes per round
roundBytes :: Int
roundBytes = 256

data Participant = Participant {
    privateKey :: PrivateKey,
    otherKeys :: [PublicKey],
    reservations :: Integer
} deriving (Eq, Show)

duplicates :: Eq a => [a] -> Bool
duplicates xs = length (nub xs) /= length xs

-- Pad the message to avoid leaking message length
padString s
    | len == roundBytes = s
    | len > roundBytes = take roundBytes s
    | len < roundBytes = s ++ replicate (roundBytes - len) ' '
    where len = length s

keyExchange :: [PrivateKey] -> [Participant]
keyExchange keys
    | duplicates keys = [] -- We should abort the key exchange if there are duplicate keys
    | otherwise = map (\k -> Participant k  [calculatePublicKey key | key <- keys, k /= key] 0) keys

generateSingleStream :: String -> PrivateKey -> [PublicKey] -> Either GenError B.ByteString
generateSingleStream message privKey keys = 
    sendMessage seeds $ Right . strBytes $ padString message
    where seeds = rights $ calculateSharedSeeds privKey keys

generateStream :: String -> Int -> Participant -> Either GenError B.ByteString
generateStream msg roundNo p
    | turnToTransmit = sendMessage [pubKey k | k <- otherKeys p] $ Right . strBytes $ padString msg
    | not turnToTransmit = sendMessage [pubKey k | k <- tail . otherKeys $ p] $ keyData . pubKey . head . otherKeys $ p
    where turnToTransmit = elem roundNo . getRounds $ reservations p

generateStreams :: String -> Int -> [Participant] -> [Either GenError B.ByteString]
generateStreams msg roundNo = map $ generateStream msg roundNo

combineStreams :: [Either GenError B.ByteString] -> B.ByteString
combineStreams streams = foldl1 strXor $ rights streams

xorStreams :: [B.ByteString] -> B.ByteString
xorStreams streams = foldl1 strXor streams

-- Round negotiation
roundSpace = roundBytes

combineRounds :: [Int] -> Integer
combineRounds rounds
    | duplicates rounds = 0
    | otherwise = foldr (\r acc -> xor acc $ setBit 0 r) 0 rounds

getRounds :: Integer -> [Int]
getRounds r = map fst $ filter snd reservations
    where isSet bits = testBit (fromIntegral bits :: Integer)
          reservations = map (\x -> (x, isSet r x)) [0..roundSpace - 1]

generateRounds :: [Participant] -> [IO Participant]
generateRounds = map setReservation

setReservation :: Participant -> IO Participant
setReservation p = do
    res <- systemRandomByte
    return p{reservations = combineRounds [res]}

