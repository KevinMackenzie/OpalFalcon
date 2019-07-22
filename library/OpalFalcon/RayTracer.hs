module OpalFalcon.RayTracer where

import OpalFalcon.Math.Vector
import qualified OpalFalcon.Math.Ray as R
import OpalFalcon.BaseTypes
import OpalFalcon.Scene


shootRay :: (ObjectCollection o) => Scene o -> R.Ray -> ColorRGBf
shootRay scene ray = traceRay scene (\r -> (rayDepth r) > 4) $ defaultRtRay ray

traceRay :: (ObjectCollection o) => Scene o -> (RtRay -> Bool) -> RtRay -> ColorRGBf
traceRay scene br ray =
    if br ray then (rayColor ray) else
    case (probeCollection (objects scene) (rayBase ray)) of
        Nothing -> rayColor ray
        Just hit -> matApply (hitMat hit) $ traceRay scene br $ MkRtR { rayBase = (R.MkRay (hitPos hit{-epsilon maybe required-}) (hitOut hit))
                                                                         , rayColor = rayColor ray
                                                                         , rayDepth = (rayDepth ray) + 1
                                                                         }

