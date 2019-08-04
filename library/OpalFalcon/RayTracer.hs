module OpalFalcon.RayTracer where

import OpalFalcon.Math.Vector
import qualified OpalFalcon.Math.Ray as R
import OpalFalcon.BaseTypes
import OpalFalcon.Scene

-- Small value to add to hits to prevent double-intersections
epsilon = 0.00001

shootRay :: (ObjectCollection o) => Scene o -> R.Ray -> ColorRGBf
shootRay scene ray = traceRay scene (\r -> (rayDepth r) > 4) $ defaultRtRay ray

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


