module Reservation where

bitsForParticipants :: Double -> Int -> Int
bitsForParticipants p n = round . (+ 0.5) $ -1 / (-1+(1-p)**(2/((-1+n')*n')))
    where n' = fromIntegral n :: Double

