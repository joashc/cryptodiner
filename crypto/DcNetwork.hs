module DcNetwork (generateMessageStream, xorStreams) where
import DiffieHellman
import Data.ByteString as B (ByteString)
import RandomBytes

-- Pad the message to avoid leaking message length
padString :: Int -> String -> String
padString maxLen s
    | len == maxLen = s
    | len > maxLen = take maxLen s
    | len < maxLen = s ++ replicate (maxLen - len) ' '
    | otherwise = replicate maxLen ' '
    where len = length s

generateMessageStream :: Int -> String -> PrivateKey -> [PublicKey] -> Either String B.ByteString
generateMessageStream byteLen message privKey keys =
    if length message == 0
        then seeds >>= sendStream byteLen
        else seeds >>= sendMessage byteLen msg
    where seeds = calculateSharedSeeds privKey keys
          msg = strBytes . padString byteLen $ message

xorStreams :: [B.ByteString] -> B.ByteString
xorStreams streams = foldl1 strXor streams
