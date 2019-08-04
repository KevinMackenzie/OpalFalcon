module OpalFalcon.Scene.Objects.Sphere where

import OpalFalcon.Math.Vector
import OpalFalcon.Math.Ray
import OpalFalcon.BaseTypes
import OpalFalcon.Material

data Sphere = MkSphere Vec3d Double

hittestSphere :: Sphere -> Ray -> Maybe Hit
hittestSphere s r =
    (calcSphereHit s r) <$> (intersectSphere s r)

calcSphereHit :: Sphere -> Ray -> Double -> Hit
calcSphereHit (MkSphere sPos _) r p =
    let hPos = pointAtParameter r p
        norm = normalize $ hPos |-| sPos
    in  MkHit { hitPos = hPos
              , hitNorm = norm
              , hitInc = r
              , hitParam = p
              , hitOut = reflectRay r norm
              -- For now
              , hitMat = mkSimpleMat (V3 0 0 0) (V3 0.5 0.5 0.5)
              }


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

-- Basic Ray-sphere intersection: will return negative results
getSphereParameters :: Sphere -> Ray -> (Maybe Double, Maybe Double)
getSphereParameters (MkSphere sPos rad) (MkRay rPos rDir) = 
    let newOrigin = rPos |-| sPos
        a = 1
        b = 2*(newOrigin |.| rDir)
        mNewOrigin = mag newOrigin
        c = (mNewOrigin * mNewOrigin) - (rad*rad) 
        d = (b*b)-(4*a*c)
    in if (d < 0) then (Nothing, Nothing) else ( 
        let sqd = sqrt d
            tp = ((-b)+sqd)/(2*a)
            tm = ((-b)-sqd)/(2*a)
            filterNeg x = if x < 0 then Nothing else Just x
        in (filterNeg tp, filterNeg tm)
       )


