module OpalFalcon.XTracing.RayTraceUtils where

import Data.Bits

import OpalFalcon.Math.Vector
import OpalFalcon.Math.Transformations
import OpalFalcon.Math.Ray
import OpalFalcon.Scene.Camera

type GlobalIllum = Vec3d -> Vec3d -> Vec3d -> (Vec3d -> Vec3d -> ColorRGBf) -> ColorRGBf
--                w    h
type ImageSize = (Int, Int)

genPixMap :: (Integral a) => a -> a -> [(a, a)]
genPixMap x2 y2 = [(x,y) | x <- [-x2..(x2-1)],
                           y <- [-y2..(y2-1)]]

genRay :: (Integral a) => Vec3d -> Double -> (a, a) -> Vec3d
genRay cDir d (x,y) = normalize $ fromHomo $ applyTransform (lookAt cDir) $ toHomoPos $ normalize $ V3 (fromIntegral x) (fromIntegral y) (-d)

genRays :: Camera -> Int -> [Ray]
genRays cam h = 
    let w = floor $ (cameraAspect cam) * (fromIntegral h)
        w2 = shiftR w 1
        h2 = shiftR h 1
        d = (1 / (tan $ cameraFOV cam)) * (fromIntegral h2)
    in  map ((Ray $ cameraPos cam) . (genRay (cameraDir cam) d)) $ genPixMap w2 h2
