module DcNetwork (generateSingleStream, xorStreams) where
import DiffieHellman
import Data.ByteString as B (ByteString)
import RandomBytes

-- Bytes per round
roundBytes :: Int
roundBytes = 256

-- Pad the message to avoid leaking message length
padString :: String -> String
padString s
    | len == roundBytes = s
    | len > roundBytes = take roundBytes s
    | len < roundBytes = s ++ replicate (roundBytes - len) ' '
    | otherwise = replicate roundBytes ' '
    where len = length s

generateSingleStream :: String -> PrivateKey -> [PublicKey] -> Either String B.ByteString
generateSingleStream message privKey keys =
    seeds >>= flip sendMessage msg
    where seeds = calculateSharedSeeds privKey keys
          msg = strBytes . padString $ message

xorStreams :: [B.ByteString] -> B.ByteString
xorStreams streams = foldl1 strXor streams
