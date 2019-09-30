module OpalFalcon.RayTraceUtils where

import Data.Bits

import OpalFalcon.Math.Vector
import OpalFalcon.Math.Transformations
import OpalFalcon.Math.Ray

genPixMap :: Integer -> Integer -> [(Integer, Integer)]
genPixMap x2 y2 = [(x,y) | x <- [-x2..(x2-1)],
                           y <- [-y2..(y2-1)] ]

genRay :: Vec3d -> Double -> (Integer, Integer) -> Vec3d
genRay cDir d (x,y) = normalize $ fromHomo $ applyTransform (lookAt cDir) $ toHomoPos $ normalize $ V3 (fromInteger x) (fromInteger y) (-d)

genRays :: Integer -> Integer -> Ray -> Double -> [Ray]
genRays w h (Ray cPos cDir) fov = 
    let w2 = shiftR w 1
        h2 = shiftR h 1
        d = (1 / (tan fov)) * (fromInteger h2)
    in  map ((Ray cPos) . (genRay cDir d)) $ genPixMap w2 h2
