module OpalFalcon.Scene.Objects.Disc where

import Data.Bits

import OpalFalcon.BaseTypes
import OpalFalcon.Math.Ray
import OpalFalcon.Math.Vector
import OpalFalcon.Math.Matrix
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

-- Generates points on an (sxs) square centered at the origin; TODO: use RNG
generateSquarePoints :: Double -> Integer -> [Vec2d]
generateSquarePoints s c = 
    let offset = s / (fromInteger c) 
        c2 = shiftR c 2
        cSet = map (\x -> offset * (1+2*(fromInteger x))) [-c2..(c2-1)]
    in  [V2 x y | x <- cSet, y <- cSet]

generateUnitDiscPoints :: Integer -> [Vec2d]
generateUnitDiscPoints c = filter (((>) 1.0) . mag) $ generateSquarePoints 2.0 c

getDiscTransform :: Disc -> Matrix4d
getDiscTransform (MkDisc (MkPlane space) _) = (translate (spacePos space)) ||*|| (lookAt $ zDir space)

transformDiscPoint :: Disc -> Vec2d -> Vec3d
transformDiscPoint d v = applyTransform3 (getDiscTransform d) $ promote3 v

getDiscPoints :: Disc -> Integer -> [Vec3d]
getDiscPoints d c = map (transformDiscPoint d) $ generateUnitDiscPoints c


