module DcNetwork (generateStream, xorStreams, calculateStream, calculateMessageStream) where
import DiffieHellman
import Data.ByteString as B (ByteString)
import RandomBytes
import Control.Applicative

-- Extend shared seed length by generating deterministic psudorandom keys
generateKeys :: Int -> [Seed] -> Either String [B.ByteString]
generateKeys len = mapM $ randomBytes len . intBytes

-- xor a message with shared keys
calculateMessageStream :: Int -> B.ByteString -> [Seed] -> Either String B.ByteString
calculateMessageStream byteLen msg seeds = foldl strXor msg <$> generateKeys byteLen seeds

-- or just xor the shared keys
calculateStream :: Int -> [Seed] -> Either String B.ByteString
calculateStream byteLen seeds = foldl1 strXor <$> generateKeys byteLen seeds

-- Pad the message to avoid leaking message length
padString :: Int -> String -> String
padString maxLen s
    | len == maxLen = s
    | len > maxLen = take maxLen s
    | len < maxLen = s ++ replicate (maxLen - len) ' '
    | otherwise = replicate maxLen ' '
    where len = length s

generateStream :: Int -> String -> PrivateKey -> [PublicKey] -> Either String B.ByteString
generateStream byteLen message privKey keys =
    if length message == 0
        then seeds >>= calculateStream byteLen
        else seeds >>= calculateMessageStream byteLen msg
    where seeds = calculateSharedSeeds privKey keys
          msg = strBytes . padString byteLen $ message

xorStreams :: [B.ByteString] -> B.ByteString
xorStreams streams = foldl1 strXor streams
