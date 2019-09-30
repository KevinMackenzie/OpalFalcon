module OpalFalcon.RayTracer (
    rayTraceScene 
    ) where

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

breakRt :: Integer -> RtRay -> Bool
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

ambientLighting :: ColorRGBf
ambientLighting = gray 0.15

traceRay :: (ObjectCollection o) => Scene o -> (RtRay -> Bool) -> RtRay -> ColorRGBf
traceRay scene br ray =
    if br ray then evaluateRayColor (sampleLights ambientLighting scene $ R.pos $ rayBase ray) black ray
    else let aRay = advanceRtRay ray
         in  case (probeCollection (objects scene) (rayBase aRay)) of
                 Nothing -> black
                 Just hit -> traceRay scene br $ deriveRay hit $ matApply (hitMat hit) hit aRay

evaluateRayColor :: ColorRGBf -> ColorRGBf -> RtRay -> ColorRGBf
evaluateRayColor i fallback ray = (rayColor ray) |*| (fromMaybe fallback (((i |*|) . (matDiffuseColor . hitMat)) <$> (rayHit ray)))

rayTraceScene :: (ObjectCollection o) => Scene o -> Integer -> Integer -> R.Ray -> Double -> [ColorRGBf]
rayTraceScene scene width height cameraRay fov = map (shootRay scene) $ genRays width height cameraRay fov

