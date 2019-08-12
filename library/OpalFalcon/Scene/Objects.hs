module OpalFalcon.Scene.Objects where

import OpalFalcon.Math.Vector
import OpalFalcon.Math.Ray
import OpalFalcon.BaseTypes as Bt
import OpalFalcon.Scene.Objects.Sphere as S
import OpalFalcon.Scene.Objects.Plane as P
import OpalFalcon.Scene.Objects.Disc as D
import OpalFalcon.Scene.Objects.PointLight as PL

-- Makes an object out of a sphere
mkSphereObject :: Vec3d -> Double -> Bt.Object
mkSphereObject p r = 
    Bt.MkObj { objPos = p
             , objIntersectRay = S.hittestSphere (MkSphere p r)
             }

mkPlaneObject :: Ray -> Bt.Object
mkPlaneObject ray@(MkRay pPos _) = 
    Bt.MkObj { objPos = pPos
             , objIntersectRay = P.hittestPlaneFront (MkPlane ray)
             }

mkDiscObject :: Plane -> Double -> Bt.Object
mkDiscObject plane@(MkPlane (MkRay p _)) radius =
    Bt.MkObj { objPos = p
             , objIntersectRay = D.hittestDisc (D.MkDisc plane radius)
             }

mkPointLight :: Vec3d -> ColorRGBf -> Float -> LightSource
mkPointLight p c d = MkLight { lightSample = PL.samplePointLight (PL.MkPL p c d) } 
