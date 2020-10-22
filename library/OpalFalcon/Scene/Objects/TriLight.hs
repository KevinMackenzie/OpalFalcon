module OpalFalcon.Scene.Objects.TriLight
  ( TriLight (MkTL),
    sampleTriLight,
    emitTriPhotons,
  )
where

import Control.Monad.Random
import OpalFalcon.BaseTypes
import OpalFalcon.Math.Lighting
import OpalFalcon.Math.Optics
import OpalFalcon.Math.Vector
import qualified OpalFalcon.Photon.PhotonTracer as PT
import OpalFalcon.Scene.Objects.PointLight
import OpalFalcon.Scene.Objects.Triangle
import qualified OpalFalcon.Util.Misc as Misc

data TriLight = MkTL Triangle ColorRGBf Float

-- NOTE: the light power is doubled because the point light is omnidirectional,
--  but the tri light emits over a hemisphere
sampleTriLight :: (Monad m, RandomGen g) => Integer -> TriLight -> RandT g m [LightSample]
sampleTriLight c (MkTL lTri lCol lPow) =
  concat
    <$> ( replicateM (fromInteger c) $
            uniformTri lTri
              >>= (\x -> samplePointLight $ MkPL x lCol (2 * lPow / (fromInteger c)))
        )

emitTriPhotons :: (Monad m, RandomGen g) => TriLight -> Int -> RandT g m [EmissivePhoton]
emitTriPhotons (MkTL lTri lCol lPow) cnt =
  do
    dirs <- Misc.repeatMF (toInteger cnt) $ cosWeightedDir $ triangleNorm lTri
    pts <- Misc.repeatMF (toInteger cnt) $ randTriPt lTri
    return
      $ map
        (\(d, pt) -> EPhoton (Ray pt $ inVec3 d) ((lCol |* lPow) |/ (fromIntegral cnt)))
      $ zip dirs pts
