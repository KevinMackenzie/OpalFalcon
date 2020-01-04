module OpalFalcon.Scene.Objects.Triangle
  ( TriangleMat,
    Triangle (MkTriangle),
    triangleNorm,
    trianglePos,
    hittestTriangleFront,
    hittestTriangle,
  )
where

import OpalFalcon.BaseTypes
import OpalFalcon.Math.Ray
import OpalFalcon.Math.Transformations
import OpalFalcon.Math.Vector

-- Note: The parameter for triangular materials is in barycentric coords.  I think this lets us interpolate across UV parameters easily for textured triangles
type TriangleMat = Triangle -> Vec3d -> AppliedMaterial

-- A triangle is an ordered set of 3 points with CCW normal ordering
data Triangle = MkTriangle !Vec3d !Vec3d !Vec3d

trianglePos :: Triangle -> Vec3d
trianglePos (MkTriangle v0 v1 v2) = (v0 |+| v1 |+| v2) |/ 3

triangleNorm :: Triangle -> Vec3d
triangleNorm (MkTriangle v0 v1 v2) = (v1 |-| v0) |><| (v2 |-| v0)

-- Hittest that ignores backface hits
hittestTriangleFront :: Triangle -> TriangleMat -> Ray -> Maybe Hit
hittestTriangleFront t mat r@(Ray _ rDir) = if ((triangleNorm t) |.| rDir) > 0 then Nothing else hittestTriangle t mat r

-- Hittests either side of a triangle:  TODO: this doesn't work for back-face intersection
-- https://www.scratchapixel.com/lessons/3d-basic-rendering/ray-tracing-rendering-a-triangle/ray-triangle-intersection-geometric-solution
hittestTriangle :: Triangle -> TriangleMat -> Ray -> Maybe Hit
hittestTriangle t@(MkTriangle v0 v1 v2) mat r@(Ray rPos rDir) =
  let n' = triangleNorm t
      n = normalize n'
      a2 = mag n' -- cross product is the double-area
      d = n |.| v0 -- the constant offset of the implicit plane function
      dDir = n |.| rDir
      dPos = n |.| rPos
      p = -(dPos - d) / dDir -- NOTE: This differs from the above implementation
      hPos = pointAtParameter r p
      vc0 = (v1 |-| v0) |><| (hPos |-| v0)
      vc1 = (v2 |-| v1) |><| (hPos |-| v1)
      vc2 = (v0 |-| v2) |><| (hPos |-| v2)
      u = (mag vc1) / a2
      v = (mag vc2) / a2
      w = (mag vc0) / a2
   in if ((abs dDir) < 0.001) || (p < 0) || ((n |.| vc0) < 0) || ((n |.| vc1) < 0) || ((n |.| vc2) < 0)
        then Nothing
        else Just MkHit
          { hitPos = hPos,
            hitNorm = n,
            hitInc = r,
            hitParam = p,
            hitMat = mat t (V3 u v w)
          }
