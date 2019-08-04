module OpalFalcon.Scene.Objects where

import OpalFalcon.Math.Vector
import OpalFalcon.Math.Ray
import OpalFalcon.BaseTypes as Bt
import OpalFalcon.Scene.Objects.Sphere as S
import OpalFalcon.Scene.Objects.Plane as P

-- Makes an object out of a sphere
mkSphereObject :: Vec3d -> Double -> Bt.Object
mkSphereObject p r = Bt.MkObj { objPos = p
                              , objIntersectRay = S.hittestSphere (MkSphere p r)
                              }

mkPlaneObject :: Ray -> Bt.Object
mkPlaneObject ray@(MkRay pPos _) = 
    Bt.MkObj { objPos = pPos
             , objIntersectRay = P.hittestPlaneFront (MkPlane ray)
             }

