import RandomBytes
import GHC.Word
import Data.ByteString as B (ByteString, unpack)
import Crypto.Random
import Control.Monad

data PrivateKey = PrivateKey {
    secret :: Integer,
    privParams :: GroupParameters
}

data PublicKey = PublicKey {
    pubKey :: Integer,
    pubParams :: GroupParameters
}

data GroupParameters = GroupParameters {
    group :: Integer,
    prime :: Integer
} deriving (Eq)

calculatePublicKey :: PrivateKey -> PublicKey
calculatePublicKey privateKey =
    PublicKey (mod (g^a) p) $ GroupParameters g p
    where a = secret privateKey
          g = group $ privParams privateKey
          p = prime $ privParams privateKey

type Seed = Integer
calculateSharedSeed :: PublicKey -> PrivateKey -> Maybe Seed
calculateSharedSeed pub priv
    | pubParams pub /= privParams priv = Nothing
    | otherwise = Just $ mod (pubKey pub ^ secret priv) $ prime $ privParams priv

sendMessage :: [Seed] -> Either GenError B.ByteString -> Either GenError B.ByteString
sendMessage seeds msg = Prelude.foldl (\acc seed -> liftM (strXor seed) acc) msg streams
    where streams = rights $ map (randomBytes 1024 . intBytes) seeds

shaInt :: Integer -> B.ByteString
shaInt = hash . intBytes

-- Utility functions for bytestring integer
byteStringInt :: B.ByteString -> Integer
byteStringInt = wordArrayInt . intWordArray . B.unpack

intWordArray :: [GHC.Word.Word8] -> [(Integer, GHC.Word.Word8)]
intWordArray array
    | len <= 7 = zipWith (\index byte -> (index, byte)) [len-1,len-2..0] array
    | otherwise = zipWith (\index byte -> (index, byte)) [len-9,len-10..0] $ drop 8 array
    where len = fromIntegral $ length array :: Integer

wordArrayInt :: [(Integer, Word8)] -> Integer
wordArrayInt = foldr (\(e, num) acc -> acc + ((fromIntegral num :: Integer) * (2^(8*e)))) 0

sendMessage :: [ByteString] -> Either GenError B.ByteString -> Either GenError B.ByteString
sendMessage seeds msg = Prelude.foldl (\acc seed -> liftM (strXor seed) acc) msg seeds
