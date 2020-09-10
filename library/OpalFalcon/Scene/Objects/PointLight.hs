module OpalFalcon.Scene.Objects.PointLight
  ( PointLight (MkPL),
    samplePointLight,
  )
where

import Control.Monad.Random
import OpalFalcon.BaseTypes
import OpalFalcon.Math.Lighting
import OpalFalcon.Math.Ray
import OpalFalcon.Math.Vector
import System.Random

-- Position, color, power
data PointLight = MkPL Vec3d ColorRGBf Float

samplePointLight :: (Monad m, RandomGen g) => PointLight -> (Ray -> Maybe Hit) -> RayBrdf -> Vec3d -> RandT g m ColorRGBf
samplePointLight (MkPL lPos lCol lPow) probe (RayBrdf brdf) oPos = return $
  case probe $ Ray lPos $ normalize $ oPos |-| lPos of
    Nothing -> black
    Just h ->
      if (hitPos h) ~= oPos
        then (brdf $ negateVec $ dir $ hitInc h) |*| (lCol |* (realToFrac (attenuate (realToFrac lPow) lPos oPos)))
        else black
