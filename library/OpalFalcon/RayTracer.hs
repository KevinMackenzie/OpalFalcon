module OpalFalcon.RayTracer where

import Data.Maybe
import Data.Bits

import OpalFalcon.Math.Vector
import qualified OpalFalcon.Math.Ray as R
import OpalFalcon.BaseTypes
import OpalFalcon.Scene
import OpalFalcon.Math.Transformations

-- Small value to add to hits to prevent double-intersections
epsilon :: Double
epsilon = 0.00001

breakRt bounces r = d > bounces || d < 0 where d = rayDepth r

shootRay :: (ObjectCollection o) => Scene o -> R.Ray -> ColorRGBf
shootRay scene ray = clamp whitef $ traceRay scene (breakRt 2) $ defaultRtRay ray

advanceRtRay :: RtRay -> RtRay
advanceRtRay r = 
    MkRtR { rayBase = R.advanceRay (rayBase r) epsilon
          , rayColor = rayColor r 
          , rayDepth = rayDepth r
          , rayHit = rayHit r
          }

deriveRay :: Hit -> RtRay -> RtRay
deriveRay h r = 
    MkRtR { rayBase = rayBase r
          , rayColor = rayColor r 
          , rayDepth = (rayDepth r) + 1
          , rayHit = Just h
          }

traceRay :: (ObjectCollection o) => Scene o -> (RtRay -> Bool) -> RtRay -> ColorRGBf
traceRay scene br ray =
    if br ray then evaluateRayColor (sampleLights scene $ R.pos $ rayBase ray) black ray
    else let aRay = advanceRtRay ray
         in  case (probeCollection (objects scene) (rayBase aRay)) of
                 Nothing -> black
                 Just hit -> traceRay scene br $ deriveRay hit $ matApply (hitMat hit) hit aRay

evaluateRayColor :: ColorRGBf -> ColorRGBf -> RtRay -> ColorRGBf
evaluateRayColor i fallback ray = (rayColor ray) |*| (fromMaybe fallback (((i |*|) . (matDiffuseColor . hitMat)) <$> (rayHit ray)))

genPixMap :: Integer -> Integer -> [(Integer, Integer)]
genPixMap x2 y2 = [(x,y) | x <- [-x2..(x2-1)],
                           y <- [-y2..(y2-1)] ]

genRay :: Vec3d -> Double -> (Integer, Integer) -> Vec3d
genRay cDir d (x,y) = normalize $ fromHomo $ applyTransform (lookAt cDir) $ toHomoPos $ normalize $ V3 (fromInteger x) (fromInteger y) (-d)

genRays :: Integer -> Integer -> R.Ray -> Double -> [R.Ray]
genRays w h (R.MkRay cPos cDir) fov = 
    let w2 = shiftR w 1
        h2 = shiftR h 1
        d = (1 / (tan fov)) * (fromInteger h2)
    in  map ((R.MkRay cPos) . (genRay cDir d)) $ genPixMap w2 h2

rayTraceScene :: (ObjectCollection o) => Scene o -> Integer -> Integer -> R.Ray -> Double -> [ColorRGBf]
rayTraceScene scene width height cameraRay fov = map (shootRay scene) $ genRays width height cameraRay fov

