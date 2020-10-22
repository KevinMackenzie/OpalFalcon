module OpalFalcon.Scene.Objects.Sphere
  ( SphereMat,
    Sphere (MkSphere),
    sphereNorm,
    hittestSphere,
    hittestSphereInside,
    lookupSphereHitMat,
    exitSphere,
  )
where

-- TODO: Most of this can be moved into "math" and only the hit-construction can stay here

import Debug.Trace
import OpalFalcon.BaseTypes
import OpalFalcon.Math.Optics
import OpalFalcon.Math.Transformations
import OpalFalcon.Math.Vector

type SphereMat = Sphere -> Vec3d -> AppliedMaterial

data Sphere = MkSphere VectorSpace Double

hittestSphere :: Sphere -> Ray -> Maybe Hit
hittestSphere s r =
  (calcSphereHit s r) <$> (intersectSphere s r)

hittestSphereInside :: Sphere -> Ray -> Maybe Hit
hittestSphereInside = hittestSphere

calcSphereHit :: Sphere -> Ray -> Double -> Hit
calcSphereHit sph r p =
  let hPos = pointAtParameter r p
   in MkHit
        { hitPos = hPos,
          hitNorm = inVec3 $ sphereNorm sph hPos,
          hitInc = r,
          hitParam = p,
          hitCoords = HitNone
        }

lookupSphereHitMat :: Sphere -> SphereMat -> Hit -> AppliedMaterial
lookupSphereHitMat sphere mat hit = mat sphere $ hitPos hit

sphereNorm :: Sphere -> Vec3d -> UVec3d
sphereNorm (MkSphere sp _) p = norm3 $ p |-| (spacePos sp)

exitSphere :: Sphere -> Ray -> Maybe Vec3d
exitSphere s@(MkSphere space rad) r@(Ray pos _) =
  if distance (spacePos space) pos > rad
    then trace ("Point " ++ (show pos) ++ " was not in sphere! (" ++ (show $ spacePos space) ++ ", r=" ++ (show rad) ++ "): " ++ (show $ distance (spacePos space) pos)) Nothing
    else case intersectSphere s r of
      Nothing -> trace "Point was in sphere, but didn't intersect...!?" undefined
      Just p -> Just $ pointAtParameter r p

-- performs sphere intersection
intersectSphere :: Sphere -> Ray -> Maybe Double
intersectSphere s r = filterNegativeParameters $ getSphereParameters s r

-- Filters negative results from intersectSphere
filterNegativeParameters :: (Maybe Double, Maybe Double) -> Maybe Double
filterNegativeParameters (p0, p1) =
  case (p0, p1) of
    (Nothing, Nothing) -> Nothing
    (Just tp, Nothing) -> Just tp
    (Nothing, Just tm) -> Just tm
    (Just tp, Just tm) -> Just $ min tp tm

-- Basic Ray-sphere intersection, interior hits, but no hits behind the ray
getSphereParameters :: Sphere -> Ray -> (Maybe Double, Maybe Double)
getSphereParameters (MkSphere space rad) (Ray rPos rDir) =
  let sPos = spacePos space
      newOrigin = rPos |-| sPos
      a = 1
      b = 2 * (newOrigin |.| rDir)
      mNewOrigin = mag newOrigin
      c = (mNewOrigin * mNewOrigin) - (rad * rad)
      d = (b * b) - (4 * a * c)
   in if (d < 0)
        then (Nothing, Nothing)
        else
          let sqd = sqrt d
              tp = ((- b) + sqd) / (2 * a)
              tm = ((- b) - sqd) / (2 * a)
              filterNeg x = if x < 0 then Nothing else Just x
           in (filterNeg tp, filterNeg tm)
