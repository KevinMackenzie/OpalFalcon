module OpalFalcon.Geometry.Curves where

import OpalFalcon.Math.Vector

linearInterpolant :: (Fractional a) => (a -> a) -> (a, a) -> Int -> [(a, a)]
linearInterpolant f (low, high) steps =
  map
    ( \i ->
        let x = low + (fromIntegral i) * (high - low) / (fromIntegral steps)
         in (x, f x)
    )
    [0 .. steps]

collinear :: (Ord b, Fractional b) => Vec3 b -> Vec3 b -> Vec3 b -> Bool
collinear a b c = (b |-| a) |><| (c |-| a) ~= origin
