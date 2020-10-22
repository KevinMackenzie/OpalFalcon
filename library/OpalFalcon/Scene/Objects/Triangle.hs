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
    randTriPt,
  )
where

-- TODO: Most of this can be moved into "geometry" and only the hit-construction can stay here

import Control.Monad.Random
import OpalFalcon.BaseTypes
import qualified OpalFalcon.Geometry.Triangle as Tri
import OpalFalcon.Math.Optics
import OpalFalcon.Math.Vector

-- Note: The parameter for triangular materials is in barycentric coords.  I think this lets us interpolate across UV parameters easily for textured triangles
type TriangleMat = Triangle -> Vec3d -> AppliedMaterial

-- A triangle is an ordered set of 3 points with CCW normal ordering
data Triangle = MkTriangle !Vec3d !Vec3d !Vec3d

trianglePos :: Triangle -> Vec3d
trianglePos (MkTriangle v0 v1 v2) = (v0 |+| v1 |+| v2) |/ 3

-- Gets the normalized triangle surface normal
triangleNorm :: Triangle -> UVec3d
triangleNorm = norm3 . triangleNorm_

-- Gets the non-normalized triangle surface normal
triangleNorm_ :: Triangle -> Vec3d
triangleNorm_ (MkTriangle v0 v1 v2) = (v1 |-| v0) |><| (v2 |-| v0)

-- Hittest that ignores backface hits
hittestTriangleFront :: Triangle -> Ray -> Maybe Hit
hittestTriangleFront t r@(Ray _ rDir) =
  if ((triangleNorm_ t) |.| rDir) >= 0
    then Nothing
    else hittestTriangle t r

-- Hittest that ignores frontface hits
hittestTriangleBack :: Triangle -> Ray -> Maybe Hit
hittestTriangleBack (MkTriangle v0 v1 v2) r =
  hittestTriangleFront (MkTriangle v0 v2 v1) r

intersectTriangle :: Triangle -> Ray -> Maybe (Double, Vec3d, Vec3d)
intersectTriangle t@(MkTriangle v0 v1 v2) r@(Ray rPos rDir) =
  let n' = triangleNorm_ t
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
hittestTriangle :: Triangle -> Ray -> Maybe Hit
hittestTriangle t r =
  case intersectTriangle t r of
    Nothing -> Nothing
    Just (p, n, b) ->
      Just MkHit
        { hitPos = pointAtParameter r p,
          hitNorm = n,
          hitInc = r,
          hitParam = p,
          hitCoords = HitLocal b
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
  (inVec3 $ triangleNorm tri) |.| (pt |-| p0) <= 0

-- Credit: Shape Distributions, ACM ToG Vol 21. No4, Oct 2002 p814
--  (https://www.cs.princeton.edu/~funk/tog02.pdf)
{-# INLINE randTriPt #-}
randTriPt :: (Monad m, RandomGen g) => Triangle -> RandT g m Vec3d
randTriPt (MkTriangle p0 p1 p2) =
  do
    r1 <- getRandom
    r2 <- getRandom
    let sr1 = sqrt r1
     in return $ ((1 - sr1) *| p0) |+| ((sr1 * (1 - r2)) *| p1) |+| ((sr1 * r1) *| p2)
