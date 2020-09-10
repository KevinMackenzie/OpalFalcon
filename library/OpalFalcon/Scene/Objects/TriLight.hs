module OpalFalcon.Scene.Objects.TriLight
  ( TriLight (MkTL),
    sampleTriLight,
  )
where

import Control.Monad.Random
import Data.Bits
import OpalFalcon.BaseTypes
import OpalFalcon.Math.Lighting
import OpalFalcon.Math.Ray
import OpalFalcon.Math.Vector
import OpalFalcon.Scene.Objects.PointLight
import OpalFalcon.Scene.Objects.Triangle
import System.Random

data TriLight = MkTL Triangle ColorRGBf Float

sampleTriLight :: (Monad m, RandomGen g) => Integer -> TriLight -> (Ray -> Maybe Hit) -> RayBrdf -> Vec3d -> RandT g m ColorRGBf
sampleTriLight c (MkTL lTri lCol lPow) probe brdf oPos =
  vecSum
    <$> ( replicateM (fromInteger c) $
            uniformTri lTri
              >>= (\x -> samplePointLight (MkPL x lCol (lPow / (fromInteger c))) probe brdf oPos)
        )
