import RandomBytes
import Data.Bits
import Data.ByteString as B (ByteString)
import Crypto.Random
import Control.Monad
import Data.Either
import Data.List (nub)

data PrivateKey = PrivateKey {
    secretExponent :: Integer,
    privParams :: GroupParameters
} deriving (Eq, Show)

data PublicKey = PublicKey {
    pubKey :: Integer,
    pubParams :: GroupParameters
} deriving (Eq, Show)

data GroupParameters = GroupParameters {
    generator :: Integer,
    prime :: Integer
} deriving (Eq, Show)

-- Modular exponentiation, b^e mod m, binary shift method
modExp :: Integer -> Integer -> Integer -> Integer
modExp _ 0 _ = 1
modExp b e m = t * modExp ((b * b) `mod` m) (shiftR e 1) m `mod` m
    where t = if testBit e 0 then b `mod` m else 1

calculatePublicKey :: PrivateKey -> PublicKey
calculatePublicKey priv =
    PublicKey (modExp g e p) $ GroupParameters g p
    where e = secretExponent priv
          g = generator $ privParams priv
          p = prime $ privParams priv

type Seed = Integer
calculateSharedSeed :: PrivateKey -> PublicKey -> Maybe Seed
calculateSharedSeed priv pub
    | pubParams pub /= privParams priv = Nothing
    | otherwise = Just $ modExp base e p
    where base = pubKey pub
          e  = secretExponent priv
          p = prime $ privParams priv

calculateSharedSeeds :: PrivateKey -> [PublicKey] -> [Maybe Seed]
calculateSharedSeeds privKey = map (calculateSharedSeed privKey)

sendMessage :: [Seed] -> Either GenError B.ByteString -> Either GenError B.ByteString
sendMessage seeds msg = foldl (\acc stream -> liftM (strXor stream) acc) msg streams
    where streams =  rights $ map keyData seeds

keyData :: Integer -> Either GenError B.ByteString
keyData = randomBytes 1024 . intBytes

data Participant = Participant {
    privateKey :: PrivateKey,
    otherKeys :: [PublicKey],
    reservations :: [(Int, Bool)]
} deriving (Eq, Show)

duplicateKeys :: [PrivateKey] -> Bool
duplicateKeys ks = length (nub ks) /= length ks

keyExchange :: [PrivateKey] -> [Participant]
keyExchange keys
    | duplicateKeys keys = [] -- We should abort the key exchange if there are duplicate keys
    | otherwise = map (\k -> Participant k  [calculatePublicKey key | key <- keys, k /= key] []) keys

ikePrime1536 :: Integer
ikePrime1536 = 2410312426921032588449453307546484504130354713523913990329474856119488749076576754484602733323213113798012715662504085855584147424137329500410406662548007860788890039236041249277284093488496789947267932079989117307478217838019853968151308254332701695838708658882744512870578566987512783995859596913569566113616427038603392109147215606778817213075633561894461195854071721331254393714297680654705637180270697070411820404503658731708777717103957968735782701633634303

generateStream :: String -> Int -> Participant -> Either GenError B.ByteString
generateStream msg roundNo p
    | turnToTransmit = sendMessage [pubKey k | k <- otherKeys $ p] $ Right . strBytes $  msg
    | not turnToTransmit = sendMessage [pubKey k | k <- tail . otherKeys $ p] $ keyData . pubKey . head . otherKeys $ p
    where turnToTransmit = head [snd r | r <- reservations p, fst r == roundNo]
