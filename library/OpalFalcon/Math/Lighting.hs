module OpalFalcon.Math.Lighting where

import OpalFalcon.Math.Vector

attenuate :: (Vector a, Floating b) => b -> a b -> a b -> b
attenuate c p0 p1 =
    let r = distance p0 p1
    in  c / (r*r)

attenuateVec :: (Vector a, Vector b, Floating c) => a c -> b c -> b c -> a c
attenuateVec v p0 p1 = fmap (\x -> attenuate x p0 p1) v
