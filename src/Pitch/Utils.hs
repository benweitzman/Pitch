module Pitch.Utils (mapWithIndex)

where

mapWithIndex :: ((Int, a) -> b) -> [a] -> [b]
mapWithIndex f = mapHelper f 0
    where mapHelper _ _ [] = []
          mapHelper f idx (x:xs) = f (idx, x):mapHelper f (idx + 1) xs