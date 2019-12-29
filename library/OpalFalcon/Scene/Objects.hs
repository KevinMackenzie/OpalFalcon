module OpalFalcon.Scene.Objects where

import OpalFalcon.BaseTypes as Bt
import OpalFalcon.Math.Transformations
import OpalFalcon.Math.Vector
import OpalFalcon.Scene.Objects.Disc as D
import OpalFalcon.Scene.Objects.DiscLight as DL
import OpalFalcon.Scene.Objects.Plane as P
import OpalFalcon.Scene.Objects.PointLight as PL
import OpalFalcon.Scene.Objects.Sphere as S
import OpalFalcon.Scene.Objects.Triangle as T

-- TODO: this is not straightforward.  Make constructors from objects and their materials.  The separation is so we can have the same objects rendered with different materials so real-time interface can use simple materials?  Or maybe we can just use the 'diffuse' component of the material?

-- Makes an object out of a sphere
mkSphereObject :: Sphere -> SphereMat -> Bt.Object
mkSphereObject sphere@(MkSphere space _) mat =
  Bt.MkObj
    { objPos = spacePos space,
      objIntersectRay = S.hittestSphere sphere mat
    }

mkPlaneObject :: Plane -> PlaneMat -> Bt.Object
mkPlaneObject plane@(MkPlane space) mat =
  Bt.MkObj
    { objPos = spacePos space,
      objIntersectRay = P.hittestPlaneFront plane mat
    }

mkDiscObject :: Disc -> DiscMat -> Bt.Object
mkDiscObject disc@(MkDisc (MkPlane space) _) mat =
  Bt.MkObj
    { objPos = spacePos space,
      objIntersectRay = D.hittestDisc disc mat
    }

mkTriangleObject :: Triangle -> TriangleMat -> Bt.Object
mkTriangleObject triangle mat =
  Bt.MkObj
    { objPos = T.trianglePos triangle,
      objIntersectRay = T.hittestTriangleFront triangle mat
    }

mkDiscLight :: D.Disc -> ColorRGBf -> Float -> LightSource
mkDiscLight d c p =
  MkLight {lightSample = DL.sampleDiscLight 10 (DL.MkDL d c p)}

mkPointLight :: Vec3d -> ColorRGBf -> Float -> LightSource
mkPointLight p c d =
  MkLight {lightSample = PL.samplePointLight (PL.MkPL p c d)}
