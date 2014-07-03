import RandomBytes
import Data.ByteString as B
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
          g = Main.group $ privParams privateKey
          p = prime $ privParams privateKey

type Seed = Integer
calculateSharedSeed :: PublicKey -> PrivateKey -> Maybe Seed
calculateSharedSeed pub priv
    | pubParams pub /= privParams priv = Nothing
    | otherwise = Just $ mod (pubKey pub ^ secret priv) $ prime $ privParams priv


sendMessage :: [ByteString] -> Either GenError B.ByteString -> Either GenError B.ByteString
sendMessage seeds msg = Prelude.foldl (\acc seed -> liftM (strXor seed) acc) msg seeds
