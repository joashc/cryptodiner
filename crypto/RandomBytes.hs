module RandomBytes (binaryDump, randomNumber, randomBytes, strXor, strBytes, intBytes, systemRandomByte, systemRandomBytes) where
import Crypto.Random.DRBG
import qualified Data.ByteString.Char8 as C
import System.Random
import Data.Binary
import Data.Bits
import Data.Binary.Put
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Numeric

strBytes :: String -> B.ByteString
strBytes = C.pack

intBytes :: Integer -> B.ByteString
intBytes i = BL.toStrict $ encode (i :: Integer)

strXor :: B.ByteString -> B.ByteString -> B.ByteString
strXor x = B.pack . B.zipWith xor x

randomBytes :: Int -> B.ByteString -> Either String B.ByteString
randomBytes len seed = do
            let gen = newGen seed :: Either GenError HashDRBG
            case gen of
                Left err -> Left . show $ err
                Right g -> do
                    let Right (bytes, _) = genBytes len g
                    return bytes

-- Generate a random byte using system-provided entropy
-- TODO: increase the output space size
systemRandomByte :: IO Int
systemRandomByte = do
    g <- newGenIO :: IO SystemRandom
    case genBytes 1 g of
        Left err -> error $ show err
        Right (randByte, _) -> return . fromIntegral . head . B.unpack $ randByte :: IO Int

randomNumber :: Int -> IO Int
randomNumber max = randomRIO (0, max)

systemRandomBytes byteLength = do
    g <- newGenIO :: IO SystemRandom
    case genBytes byteLength g of
        Left err -> error $ show err
        Right (randByte, _) -> return . fst . head . readHex . concatMap (`showHex` "") . B.unpack $ randByte

binaryDump :: Int -> Int -> Maybe B.ByteString
binaryDump max x
    | max < x = Nothing
    | otherwise = Just . BL.toStrict . runPut . mapM_ (putWord8 . fromIntegral) $ padding ++ bytes
    where modulus = mod x 8
          offset = 2 ^ modulus
          shift = div x 8
          padding = replicate (max - shift) 0
          bytes = (:) offset $ replicate shift 0

indexedWord8s :: B.ByteString -> [(Int, Word8)]
indexedWord8s = filter ((> 0) . snd) . zip [0..] . reverse . B.unpack

toggledBits :: Int -> Word8 -> [Int]
toggledBits index word = map (+ offset) setBits
    where setBits = filter (testBit word) [0..8]
          offset = index * 8

toggledBitsBS :: B.ByteString -> [Int]
toggledBitsBS = concatMap (uncurry toggledBits) . indexedWord8s

type Reservation = Int
type Round = Int

getRounds :: [Reservation] -> [Reservation] -> [Round]
getRounds own grp = map fst $ filter (inGroup . snd) groupRes
    where groupRes = zip [1..] grp
          inGroup = flip elem own
