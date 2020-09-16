module OpalFalcon.Scene.Objects.TriLight
  ( TriLight (MkTL),
    sampleTriLight,
  )
where

import Control.Monad.Random
import OpalFalcon.BaseTypes
import OpalFalcon.Math.Lighting
import OpalFalcon.Math.Vector
import OpalFalcon.Scene.Objects.PointLight
import OpalFalcon.Scene.Objects.Triangle

data TriLight = MkTL Triangle ColorRGBf Float

-- TODO: This uses the point-light estimate, but a hemisphereical light aligns better with how the photons are shot from the surface
sampleTriLight :: (Monad m, RandomGen g) => Integer -> TriLight -> RandT g m [LightSample]
sampleTriLight c (MkTL lTri lCol lPow) =
  concat <$> (replicateM (fromInteger c) $
    uniformTri lTri
      >>= (\x -> samplePointLight $ MkPL x lCol (lPow / (fromInteger c))))
