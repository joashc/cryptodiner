module RandomBytes (randomBytes, strBytes, intBytes) where
import Crypto.Random
import Crypto.Random.DRBG
import Data.ByteString.Char8
import Data.Binary 
import Data.ByteString as B
import Data.ByteString.Lazy as BL

strBytes :: String -> B.ByteString
strBytes = Data.ByteString.Char8.pack

intBytes :: Integer -> B.ByteString
intBytes i = toStrict $ encode (i :: Integer) 

-- How do you get a stream?
randomBytes :: Int -> B.ByteString -> Either GenError B.ByteString
randomBytes len seed = do
            gen <- newGen seed :: Either GenError HashDRBG
            let Right (bytes, _) = genBytes len gen
            return bytes
