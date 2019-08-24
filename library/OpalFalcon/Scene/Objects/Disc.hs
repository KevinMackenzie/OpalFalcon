module OpalFalcon.Scene.Objects.Disc (
    DiscMat,
    Disc(MkDisc),
    hittestDisc,
    ) where

import OpalFalcon.BaseTypes
import OpalFalcon.Math.Ray
import OpalFalcon.Math.Vector
import OpalFalcon.Math.Transformations
import OpalFalcon.Scene.Objects.Plane

type DiscMat = Disc -> Vec3d -> AppliedMaterial
-- A plane bounded to a radius
data Disc = MkDisc Plane Double

discMatToPlaneMat :: Disc -> DiscMat -> PlaneMat
discMatToPlaneMat (MkDisc _ d) mat plane = mat (MkDisc plane d)

hittestDisc :: Disc -> DiscMat -> Ray -> Maybe Hit
hittestDisc q@(MkDisc p _) mat r =
    case (hittestPlane p (discMatToPlaneMat q mat) r) of
        Nothing -> Nothing
        Just h -> cropPlaneHit q h

cropPlaneHit :: Disc -> Hit -> Maybe Hit
cropPlaneHit (MkDisc (MkPlane space) r) h = if (distance (spacePos space) $ hitPos h) < r then Just h else Nothing

