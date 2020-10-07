module OpalFalcon.Scene.Objects.Triangle
  ( TriangleMat,
    Triangle (MkTriangle),
    triangleNorm,
    trianglePos,
    hittestTriangleFront,
    hittestTriangleBack,
    hittestTriangle,
    pointInTriangle,
    intersectTriangle,
    insideTriPlane,
  )
where

-- TODO: Most of this can be moved into "math" and only the hit-construction can stay here

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
hittestTriangleFront t mat r@(Ray _ rDir) =
  if ((triangleNorm t) |.| rDir) >= 0
    then Nothing
    else hittestTriangle t mat r

-- Hittest that ignores frontface hits
hittestTriangleBack :: Triangle -> TriangleMat -> Ray -> Maybe Hit
hittestTriangleBack (MkTriangle v0 v1 v2) mat r =
  hittestTriangleFront (MkTriangle v0 v2 v1) mat r

intersectTriangle :: Triangle -> Ray -> Maybe (Double, Vec3d, Vec3d)
intersectTriangle t@(MkTriangle v0 v1 v2) r@(Ray rPos rDir) =
  let n' = triangleNorm t
      n = normalize n'
      a2 = mag n' -- cross product is the double-area
      d = n |.| v0 -- the constant offset of the implicit plane function
      dDir = n |.| rDir
      dPos = n |.| rPos
      p = - (dPos - d) / dDir -- NOTE: This differs from the above implementation
      hPos = pointAtParameter r p
      vc0 = (v1 |-| v0) |><| (hPos |-| v0)
      vc1 = (v2 |-| v1) |><| (hPos |-| v1)
      vc2 = (v0 |-| v2) |><| (hPos |-| v2)
      u = (mag vc1) / a2
      v = (mag vc2) / a2
      w = (mag vc0) / a2
   in if ((abs dDir) < 0.001) || (p < 0) || ((n |.| vc0) < 0) || ((n |.| vc1) < 0) || ((n |.| vc2) < 0)
        then Nothing
        else Just (p, n, V3 u v w)

-- Hittests the front side of the triangle
-- https://www.scratchapixel.com/lessons/3d-basic-rendering/ray-tracing-rendering-a-triangle/ray-triangle-intersection-geometric-solution
hittestTriangle :: Triangle -> TriangleMat -> Ray -> Maybe Hit
hittestTriangle t@(MkTriangle v0 v1 v2) mat r@(Ray rPos rDir) =
  case intersectTriangle t r of
    Nothing -> Nothing
    Just (p, n, b) ->
      Just MkHit
        { hitPos = pointAtParameter r p,
          hitNorm = n,
          hitInc = r,
          hitParam = p,
          hitMat = mat t b
        }

-- The point doesn't have to be on the triangle (can be above / below)
{-# INLINE pointInTriangle #-}
pointInTriangle :: Triangle -> Vec3d -> Bool
pointInTriangle (MkTriangle p0 p1 p2) pt =
  let v0 = p1 |-| p0
      v1 = p2 |-| p0
      v2 = p2 |-| p1
   in (v2 |><| (pt |-| p1)) |.| (v0 |><| v1) > 0

{-# INLINE insideTriPlane #-}
insideTriPlane :: Triangle -> Vec3d -> Bool
insideTriPlane tri@(MkTriangle p0 _ _) pt =
  (triangleNorm tri) |.| (pt |-| p0) <= 0
