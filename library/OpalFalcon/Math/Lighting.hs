module OpalFalcon.Math.Lighting where

import OpalFalcon.Math.Vector

attenuate :: (Vector a, Floating b) => b -> a b -> a b -> b
attenuate c p0 p1 =
    let r = distance p0 p1
    in  c / (r*r)

