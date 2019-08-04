module OpalFalcon.Scene.Objects.Plane where

import OpalFalcon.Math.Ray
import OpalFalcon.Math.Vector
import OpalFalcon.BaseTypes
import OpalFalcon.Material

data Plane = MkPlane Ray

-- Hittest that ignores backface hits
hittestPlaneFront :: Plane -> Ray -> Maybe Hit
hittestPlaneFront p@(MkPlane (MkRay _ pDir)) r@(MkRay _ rDir) = if (pDir |.| rDir) > 0 then Nothing else hittestPlane p r

-- Hittests either side of a plane
hittestPlane :: Plane -> Ray -> Maybe Hit
hittestPlane (MkPlane (MkRay pPos pDir)) r@(MkRay rPos rDir) =
    let d = pDir |.| rDir
        relPos = pPos |-| rPos
        p = (relPos |.| pDir) / d
    in  if (abs d) < 0.000001 || p < 0 then Nothing
        else Just MkHit { hitPos = pointAtParameter r p
                        , hitNorm = pDir
                        , hitInc = r
                        , hitParam = p
                        , hitMat = mkDiffuseMat (V3 0.5 0.5 0.5)
                        }

