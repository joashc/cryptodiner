module RandomBytes (randomBytes, strXor, strBytes, intBytes, systemRandomByte) where
import Crypto.Random
import Crypto.Random.DRBG
import Data.ByteString.Char8
import Data.Binary 
import Data.Bits 
import Data.ByteString as B
import Data.ByteString.Lazy as BL

strBytes :: String -> B.ByteString
strBytes = Data.ByteString.Char8.pack

intBytes :: Integer -> B.ByteString
intBytes i = toStrict $ encode (i :: Integer) 


strXor :: B.ByteString -> B.ByteString -> B.ByteString
strXor x = B.pack . B.zipWith xor x

-- How do you get a stream?
randomBytes :: Int -> B.ByteString -> Either GenError B.ByteString
randomBytes len seed = do
            gen <- newGen seed :: Either GenError HashDRBG
            let Right (bytes, _) = genBytes len gen
            return bytes

-- Generate a random byte using system-provided entropy
-- TODO: increase the output space size
systemRandomByte :: IO Int
systemRandomByte = do
    g <- newGenIO :: IO SystemRandom
    case genBytes 1 g of
        Left err -> error $ show err
        Right (randByte, _) -> return $ fromIntegral . Prelude.head . B.unpack $ randByte :: IO Int


