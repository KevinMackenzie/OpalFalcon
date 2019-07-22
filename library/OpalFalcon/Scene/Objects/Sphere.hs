module OpalFalcon.Scene.Objects.Sphere where

import OpalFalcon.Math.Vector
import OpalFalcon.Math.Ray
import OpalFalcon.BaseTypes
import OpalFalcon.Material

data Sphere = MkSphere Vec3d Double

hittestSphere :: Sphere -> Ray -> Maybe Hit
hittestSphere s@(MkSphere sPos _) r =
    case intersectSphere s r of
        Nothing -> Nothing
        Just (tp, tm) -> Just (MkHit { hitPos = hPos
                                     , hitNorm = norm
                                     , hitInc = r
                                     , hitParam  = p
                                     , hitOut = reflectRay r norm
                                     -- For now
                                     , hitMat = mkSimpleMat (V3 0 0 0) (V3 1.0 0.4 1.0)
                                     }
                              )
                         where p = (min tp tm)
                               hPos = pointAtParameter r p
                               norm = hPos |-| sPos

-- Basic Ray-sphere intersection
intersectSphere :: Sphere -> Ray -> Maybe (Double, Double)
intersectSphere (MkSphere sPos rad) (MkRay rPos rDir) = 
    let newOrigin = rPos |-| sPos
        a = -1
        b = (-2)*(newOrigin |.| rDir)
        c = (rad*rad) - (newOrigin |.| newOrigin)
        d = (b*b)-(4*a*c)
    in if (d < 0) then Nothing  else ( 
        let sqd = sqrt d
            tp = ((-b)+sqd)/(2*a)
            tm = ((-b)-sqd)/(2*a)
        in Just (tp, tm)
       )


