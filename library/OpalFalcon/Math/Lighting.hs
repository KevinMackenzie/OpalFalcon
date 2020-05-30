module OpalFalcon.Math.Lighting
  ( attenuate,
    attenuateVec,
    cosWeightedDir,
    uniformDir,
    uniformHemisphere,
  )
where

import Control.Monad.Random
import OpalFalcon.Math.Matrix
import OpalFalcon.Math.Vector

attenuate :: (Vector a, Floating b) => b -> a b -> a b -> b
attenuate c p0 p1 =
  let r = distance p0 p1
   in c / (r * r)

attenuateVec :: (Vector a, Vector b, Floating c) => a c -> b c -> b c -> a c
attenuateVec v p0 p1 = fmap (\x -> attenuate x p0 p1) v

-- Gets a random cosine-weighted direction around a normal
cosWeightedDir :: (Monad m, RandomGen g, Random c, Floating c, Ord c) => Vec3 c -> RandT g m (Vec3 c)
cosWeightedDir norm = do
  rand0 <- getRandom
  rand1 <- getRandom
  return $
    let v = fromSphere $ V3 1 (acos $ sqrt rand0) (2 * pi * rand1)
        xAx = getOrthoVec norm
        yAx = norm |><| xAx
     in (V3 xAx yAx norm) ||*| v

uniformDir :: (Monad m, RandomGen g, Random c, Floating c, Ord c) => RandT g m (Vec3 c)
uniformDir = getRandom

uniformHemisphere :: (Monad m, RandomGen g, Random c, Floating c, Ord c) => Vec3 c -> RandT g m (Vec3 c)
uniformHemisphere norm = (clampHemisphere norm) <$> uniformDir
