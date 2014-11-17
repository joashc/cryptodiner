{-# LANGUAGE DeriveGeneric #-}
module DiffieHellman (PrivateKey(..), PublicKey(..), GroupParameters(..), Seed, calculatePublicKey, calculateSharedSeed, sendMessage, keyData, calculateSharedSeeds, sendStream) where
import RandomBytes
import GHC.Generics (Generic)
import Data.Bits
import Data.ByteString as B (ByteString)
import Control.Applicative

data PrivateKey = PrivateKey {
    secretExponent :: Integer,
    privParams :: GroupParameters
} deriving (Eq, Show)

data PublicKey = PublicKey {
    pubKey :: Integer,
    pubParams :: GroupParameters
} deriving (Eq, Show, Generic)

data GroupParameters = GroupParameters {
    generator :: Integer,
    prime :: Integer
} deriving (Eq, Show, Generic)

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
calculateSharedSeed :: PrivateKey -> PublicKey -> Either String Seed
calculateSharedSeed priv pub
    | pubParams pub /= privParams priv = Left "Error calculating shared seed"
    | otherwise = Right $ modExp base e p
    where base = pubKey pub
          e  = secretExponent priv
          p = prime $ privParams priv

calculateSharedSeeds :: PrivateKey -> [PublicKey] -> Either String [Seed]
calculateSharedSeeds privKey pubKeys = sequence $ calculateSharedSeed privKey <$> pubKeys

sendMessage :: Int -> B.ByteString -> [Seed] -> Either String B.ByteString
sendMessage byteLen msg seeds = foldl strXor msg <$> keyData byteLen seeds

sendStream :: Int -> [Seed] -> Either String B.ByteString
sendStream byteLen seeds = foldl1 strXor <$> keyData byteLen seeds

keyData :: Int -> [Seed] -> Either String [B.ByteString]
keyData len = mapM $ randomBytes len . intBytes
