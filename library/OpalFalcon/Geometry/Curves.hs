module OpalFalcon.Geometry.Curves where

linearInterpolant :: (Fractional a) => (a -> a) -> (a, a) -> Int -> [(a, a)]
linearInterpolant f (low, high) steps =
  map
    ( \i ->
        let x = low + (fromIntegral i) * (high - low) / (fromIntegral steps)
         in (x, f x)
    )
    [0 .. steps]
