import Crypto.Random
import Crypto.Random.DRBG
import Data.ByteString.Char8

strBytes :: String -> ByteString
strBytes = Data.ByteString.Char8.pack

-- How do you get a stream?
randomBytes :: Int -> ByteString -> Either GenError ByteString
randomBytes len seed = do
            gen <- newGen seed :: Either GenError HashDRBG
            let Right (bytes, _) = genBytes len gen
            return bytes
