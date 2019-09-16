module OpalFalcon.Math.Lighting where

import OpalFalcon.Math.Vector
import qualified Data.Vector.Fixed.Primitive as VPrim

attenuate :: (VPrim.Prim a, Floating a) => a -> Vec3 a-> Vec3 a -> a
attenuate c p0 p1 =
    let r = distance p0 p1
    in  c / (r*r)

