module OpalFalcon.Scene.Objects.PointLight
  ( PointLight (MkPL),
    samplePointLight,
  )
where

import Control.Monad.Random
import GHC.Float (double2Float)
import OpalFalcon.BaseTypes
import OpalFalcon.Math.Lighting
import OpalFalcon.Math.Ray
import OpalFalcon.Math.Vector
import System.Random

-- Position, color, power
data PointLight = MkPL Vec3d ColorRGBf Float

samplePointLight :: (Monad m, RandomGen g) => PointLight -> RandT g m [LightSample]
samplePointLight (MkPL lPos lCol lPow) = return [LightSample lPos (AttenuationFunc $ (double2Float) . (attenuate (realToFrac lPow) lPos)) lCol]
