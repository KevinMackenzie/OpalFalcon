module OpalFalcon.Scene.Objects.PointLight where

import OpalFalcon.BaseTypes
import OpalFalcon.Math.Ray
import OpalFalcon.Math.Vector

-- Position, color, power
data PointLight = MkPL Vec3d ColorRGBf Float

samplePointLight :: PointLight -> (Ray -> Maybe Hit) -> Vec3d -> ColorRGBf
samplePointLight (MkPL lPos lCol lPow) probe oPos = 
    case probe $ MkRay lPos $ normalize $ oPos |-| lPos of
        Nothing -> black
        Just h -> if (hitPos h) ~= oPos 
                      then let r = realToFrac (distance lPos oPos) in (lPow / (r*r)) *| lCol
                      else black

