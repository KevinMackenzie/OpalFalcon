module OpalFalcon.Math.Lighting
  ( attenuate,
    attenuateVec,
    cosWeightedDir,
  )
where

import Control.Monad.Random
import OpalFalcon.Math.Vector

attenuate :: (Vector a, Floating b) => b -> a b -> a b -> b
attenuate c p0 p1 =
  let r = distance p0 p1
   in c / (r * r)

attenuateVec :: (Vector a, Vector b, Floating c) => a c -> b c -> b c -> a c
attenuateVec v p0 p1 = fmap (\x -> attenuate x p0 p1) v

cosWeightedDir :: (Monad m, RandomGen g, Random c, Floating c) => Vec3 c -> RandT g m (Vec3 c)
cosWeightedDir norm = do
  rand0 <- getRandom
  rand1 <- getRandom
  return $ fromSphere $ V3 1 (acos $ sqrt rand0) (2 * pi * rand1)
