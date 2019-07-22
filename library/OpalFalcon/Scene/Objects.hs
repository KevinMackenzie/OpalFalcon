module OpalFalcon.Scene.Objects where

import OpalFalcon.Math.Vector
import OpalFalcon.BaseTypes as Bt
import OpalFalcon.Scene.Objects.Sphere as S

-- Makes an object out of a sphere
mkSphereObject :: Vec3d -> Double -> Bt.Object
mkSphereObject p r = Bt.MkObj { objPos = p
                              , objIntersectRay = S.hittestSphere (MkSphere p r)
                              }

