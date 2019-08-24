module OpalFalcon.Scene.Objects.Plane (
    PlaneMat,
    Plane(MkPlane),
    hittestPlaneFront,
    hittestPlane
    ) where

import OpalFalcon.Math.Ray
import OpalFalcon.Math.Vector
import OpalFalcon.Math.Transformations
import OpalFalcon.BaseTypes

type PlaneMat = Plane -> Vec3d -> AppliedMaterial
-- A plane is a a vector space in the positive z direction
newtype Plane = MkPlane VectorSpace

-- Hittest that ignores backface hits
hittestPlaneFront :: Plane -> PlaneMat -> Ray -> Maybe Hit
hittestPlaneFront p@(MkPlane space) mat r@(MkRay _ rDir) = if ((zDir space) |.| rDir) > 0 then Nothing else hittestPlane p mat r

-- Hittests either side of a plane
hittestPlane :: Plane -> PlaneMat -> Ray -> Maybe Hit
hittestPlane plane@(MkPlane space) mat r@(MkRay rPos rDir) =
    let pDir = zDir space
        pPos = spacePos space
        d = pDir |.| rDir
        relPos = pPos |-| rPos
        p = (relPos |.| pDir) / d
        hPos = pointAtParameter r p
    in  if (abs d) < 0.000001 || p < 0 then Nothing
        else Just MkHit { hitPos = hPos
                        , hitNorm = pDir
                        , hitInc = r
                        , hitParam = p
                        , hitMat = mat plane hPos
                        }

