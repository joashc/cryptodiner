module DcNetwork (Participant(..), keyExchange, generateStream, generateStreams, combineStreams) where
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

generateStream :: String -> Int -> Participant -> Either GenError B.ByteString
generateStream msg roundNo p
    | turnToTransmit = sendMessage [pubKey k | k <- otherKeys p] $ Right . strBytes $ padString msg
    | not turnToTransmit = sendMessage [pubKey k | k <- tail . otherKeys $ p] $ keyData . pubKey . head . otherKeys $ p
    where turnToTransmit = elem roundNo . getRounds $ reservations p

generateStreams :: String -> Int -> [Participant] -> [Either GenError B.ByteString]
generateStreams msg roundNo = map $ generateStream msg roundNo

combineStreams :: [Either GenError B.ByteString] -> B.ByteString
combineStreams streams = foldl1 strXor $ rights streams

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

-- Testing

ikePrime1536 :: Integer
ikePrime1536 = 2410312426921032588449453307546484504130354713523913990329474856119488749076576754484602733323213113798012715662504085855584147424137329500410406662548007860788890039236041249277284093488496789947267932079989117307478217838019853968151308254332701695838708658882744512870578566987512783995859596913569566113616427038603392109147215606778817213075633561894461195854071721331254393714297680654705637180270697070411820404503658731708777717103957968735782701633634303

-- Allow first participant to reserve first slot
enableParticipant :: [Participant] -> [Participant]
enableParticipant (x:xs) = x{reservations = 0}:xs

gp = GroupParameters 2 ikePrime1536
privateKeys = map (`PrivateKey` gp) [34980320895483925743295723859734250743809534780257324859072347839573248543857324543548390,554908390050423879572389574328574320957823495734857324862147813975414567857412807628905623862385672318,234234252371857856321789507259430854308095348039485303856128738956321523185372853478543,2082390543895734845635976210652189578957843651056471235648397340343075834,904685965045483256789045432954602356342754378925738245723485743257234856438]
participants = enableParticipant . keyExchange $ privateKeys

-- Get a stream from each participant
streams = generateStreams "hello!!!!!!" 0 participants

-- Recombine all the streams
result = combineStreams streams

