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
          g = group $ privParams privateKey
          p = prime $ privParams privateKey

type Seed = Integer
calculateSharedSeed :: PublicKey -> PrivateKey -> Maybe Seed
calculateSharedSeed pub priv
    | pubParams pub /= privParams priv = Nothing
    | otherwise = Just $ mod (pubKey pub ^ secret priv) $ prime $ privParams priv

