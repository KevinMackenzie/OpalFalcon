module OpalFalcon.RayTracer where

import OpalFalcon.Math.Vector
import qualified OpalFalcon.Math.Ray as R
import OpalFalcon.BaseTypes
import OpalFalcon.Scene
import Data.Bits
import OpalFalcon.Math.Transformations

-- Small value to add to hits to prevent double-intersections
epsilon :: Double
epsilon = 0.00001

shootRay :: (ObjectCollection o) => Scene o -> R.Ray -> ColorRGBf
shootRay scene ray = traceRay scene (\r -> (rayDepth r) > 4) $ defaultRtRay ray

deriveRay :: RtRay -> Hit -> RtRay
deriveRay r h = MkRtR { rayBase = R.advanceRay (R.MkRay (hitPos h) (hitOut h)) epsilon
                      , rayColor = rayColor r 
                      , rayDepth = (rayDepth r) + 1
                      }

traceRay :: (ObjectCollection o) => Scene o -> (RtRay -> Bool) -> RtRay -> ColorRGBf
traceRay scene br ray =
    if br ray then (rayColor ray) else
    case (probeCollection (objects scene) (rayBase ray)) of
        Nothing -> rayColor ray
        Just hit ->  matApply (hitMat hit) $ traceRay scene br $ deriveRay ray hit


genPixMap :: Integer -> Integer -> [(Integer, Integer)]
genPixMap x2 y2 = [(x,y) | x <- [-x2..(x2-1)],
                           y <- [-y2..(y2-1)] ]

genRays :: Integer -> Integer -> R.Ray -> Double -> [R.Ray]
genRays w h (R.MkRay cPos cDir) fov = 
    let w2 = shiftR w 1
        h2 = shiftR h 1
        d = (1 / (tan fov)) * (fromInteger h2)
        pxMap = genPixMap w2 h2
        dirs = map (\(x,y) -> noHomo (normalize (
                                applyTransform (toHomoPos (normalize (V3 (fromInteger x) (fromInteger y) (-d)))) 
                                               (lookAt cDir)
                              ))) pxMap
    in  map (R.MkRay cPos) dirs

rayTraceScene :: (ObjectCollection o) => Scene o -> Integer -> Integer -> R.Ray -> Double -> [ColorRGBf]
rayTraceScene scene width height cameraRay fov = map (shootRay scene) $ genRays width height cameraRay fov

