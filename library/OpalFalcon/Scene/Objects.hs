module OpalFalcon.Scene.Objects where

import OpalFalcon.BaseTypes as Bt
import OpalFalcon.Math.Transformations
import qualified OpalFalcon.Math.TriMesh as TMesh
import OpalFalcon.Math.Vector
import OpalFalcon.Scene.Objects.Disc as D
import OpalFalcon.Scene.Objects.DiscLight as DL
import OpalFalcon.Scene.Objects.Plane as P
import OpalFalcon.Scene.Objects.PointLight as PL
import OpalFalcon.Scene.Objects.Sphere as S
import OpalFalcon.Scene.Objects.TriLight as TL
import OpalFalcon.Scene.Objects.Triangle as T

-- TODO: this is not straightforward.  Make constructors from objects and their materials.  The separation is so we can have the same objects rendered with different materials so real-time interface can use simple materials?  Or maybe we can just use the 'diffuse' component of the material?

-- Makes an object out of a sphere
mkSphereObject :: Sphere -> SphereMat -> Bt.Object
mkSphereObject sphere@(MkSphere space _) mat =
  Bt.MkObj
    { objPos = spacePos space,
      objIntersectRay = S.hittestSphere sphere,
      objHitMat = (\h -> mat sphere (hitPos h)),
      objLightSource = Nothing
    }

mkPlaneObject :: Plane -> PlaneMat -> Bt.Object
mkPlaneObject plane@(MkPlane space) mat =
  Bt.MkObj
    { objPos = spacePos space,
      objIntersectRay = P.hittestPlaneFront plane,
      objHitMat = (\h -> mat plane (hitPos h)),
      objLightSource = Nothing
    }

mkDiscObject :: Disc -> DiscMat -> Bt.Object
mkDiscObject disc@(MkDisc (MkPlane space) _) mat =
  Bt.MkObj
    { objPos = spacePos space,
      objIntersectRay = D.hittestDisc disc,
      objHitMat = (\h -> mat disc (hitPos h)),
      objLightSource = Nothing
    }

mkTriangleObject :: Triangle -> TriangleMat -> Bt.Object
mkTriangleObject triangle mat =
  Bt.MkObj
    { objPos = T.trianglePos triangle,
      objIntersectRay = T.hittestTriangleFront triangle,
      objHitMat = (\h -> mat triangle $ hitLocal h),
      objLightSource = Nothing
    }

mkTriangleObjectFull :: Triangle -> TriangleMat -> ColorRGBf -> Float -> Bt.Object
mkTriangleObjectFull t mat col pow =
  Bt.MkObj
    { objPos = T.trianglePos t,
      objIntersectRay = T.hittestTriangleFront t,
      objHitMat = (\h -> mat t $ hitLocal h),
      objLightSource = Just $ MkLight
        { lightSample = TL.sampleTriLight 20 (TL.MkTL t col pow),
          emitPhotons = TL.emitTriPhotons (TL.MkTL t col pow)
        }
    }

mkTriMeshObject :: TMesh.TriMesh -> TMesh.TriMeshMat -> Bt.Object
mkTriMeshObject mesh mat =
  Bt.MkObj
    { objPos = origin, -- TODO
      objIntersectRay = TMesh.hittestTriMeshOutside mesh,
      objHitMat = (\h -> uncurry (mat mesh) $ hitIndexed h),
      objLightSource = Nothing
    }

mkDiscLight :: D.Disc -> ColorRGBf -> Float -> LightSource
mkDiscLight d c p =
  MkLight
    { lightSample = DL.sampleDiscLight 10 (DL.MkDL d c p),
      emitPhotons = undefined
    }

mkPointLight :: Vec3d -> ColorRGBf -> Float -> LightSource
mkPointLight p c d =
  MkLight
    { lightSample = PL.samplePointLight (PL.MkPL p c d),
      emitPhotons = undefined
    }
