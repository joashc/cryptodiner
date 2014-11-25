module DcNetwork (encodeMessage, generateStream, xorStreams, calculateStream, calculateMessageStream) where
import DiffieHellman
import DhGroupParams
import qualified Data.ByteString as B (ByteString, concat, length)
import RandomBytes
import Data.List (foldl', foldl1')
import Control.Applicative
import qualified Control.Monad as M (join)
import Data.Serialize

-- Extend shared seed length by generating deterministic psudorandom keys
generateKeys :: Int -> [Seed] -> Either String [B.ByteString]
generateKeys len = mapM $ randomBytes len . intBytes

-- xor a message with shared keys
calculateMessageStream :: Int -> B.ByteString -> [Seed] -> Either String B.ByteString
calculateMessageStream byteLen msg seeds = foldl' strXor msg <$> generateKeys byteLen seeds

-- or just xor the shared keys
calculateStream :: Int -> [Seed] -> Either String B.ByteString
calculateStream byteLen seeds = foldl1' strXor <$> generateKeys byteLen seeds

-- Pad the message to avoid leaking message length
padString :: Int -> String -> String
padString maxLen s
    | len == maxLen = s
    | len > maxLen = take maxLen s
    | len < maxLen = s ++ replicate (maxLen - len) ' '
    | otherwise = replicate maxLen ' '
    where len = length s + 8

generateStream :: Int -> String -> PrivateKey -> [PublicKey] -> Either String B.ByteString
generateStream byteLen message privKey keys =
    if length message == 0
        then calculateStream byteLen =<< seeds
        else M.join $ calculateMessageStream byteLen <$> msgBody <*> seeds
    where seeds = calculateSharedSeeds privKey $ excludeSelf privKey keys
          msgBody = encodeMessage byteLen message

-- Encodes a given string to a bytestring padded with a given length,
-- with the actual message length encoded in the first 8 bytes of the
-- bytestring
encodeMessage :: Int -> String -> Either String B.ByteString
encodeMessage byteLen message =
    let msgLen = encode . length $ message
        msg = strBytes . padString byteLen $ message
    in if B.length msgLen == 8 -- 8 bytes should be enough for anyone
    then Right . B.concat $ [msgLen, msg]
    else Left "Invalid message size"

excludeSelf :: PrivateKey -> [PublicKey] -> [PublicKey]
excludeSelf priv pubs = filter (\pub -> pub /= calculatePublicKey priv) pubs

xorStreams :: [B.ByteString] -> B.ByteString
xorStreams streams = foldl1' strXor streams
