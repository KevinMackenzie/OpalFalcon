{-# LANGUAGE BangPatterns #-}

module OpalFalcon.Math.Lighting
  ( attenuate,
    attenuateVec,
    cosWeightedDir,
    uniformDir,
    uniformHemisphere,
    uniformTri,
  )
where

import Control.Monad.Random
import OpalFalcon.Math.Matrix
import OpalFalcon.Math.Vector
import OpalFalcon.Scene.Objects.Triangle (Triangle (..), pointInTriangle)

{-# INLINE attenuate #-}
attenuate :: (Vector a, Floating b) => b -> a b -> a b -> b
attenuate c p0 p1 =
  let r = distance p0 p1
   in c / (4 * pi * r * r)

{-# INLINE attenuateVec #-}
attenuateVec :: (Vector a, Vector b, Floating c) => a c -> b c -> b c -> a c
attenuateVec v p0 p1 = fmap (\x -> attenuate x p0 p1) v

-- Gets a random cosine-weighted direction around a normal
{-# INLINE cosWeightedDir #-}
cosWeightedDir :: (Monad m, RandomGen g, Random c, Floating c, Ord c) => Vec3 c -> RandT g m (Vec3 c)
cosWeightedDir norm = do
  rand0 <- getRandom
  rand1 <- getRandom
  return $
    let v = fromSphere $ V3 1 (acos $ sqrt rand0) (2 * pi * rand1)
        xAx = getOrthoVec norm
        yAx = norm |><| xAx
     in (V3 xAx yAx norm) ||*| v

{-# INLINE uniformDir #-}
uniformDir :: (Monad m, RandomGen g, Random c, Floating c, Ord c) => RandT g m (Vec3 c)
uniformDir = getRandom

{-# INLINE uniformHemisphere#-}
uniformHemisphere :: (Monad m, RandomGen g, Random c, Floating c, Ord c) => Vec3 c -> RandT g m (Vec3 c)
uniformHemisphere norm = (clampHemisphere norm) <$> uniformDir

-- Source: https://mathworld.wolfram.com/TrianglePointPicking.html
{-# INLINE uniformTri #-}
uniformTri :: (Monad m, RandomGen g) => Triangle -> RandT g m Vec3d
uniformTri t@(MkTriangle p0 p1 p2) =
  do
    r0 <- getRandom
    r1 <- getRandom
    let !pt = (r0 *| (p1 |-| p0)) |+| (r1 *| (p2 |-| p0)) |+| p0
     in if pointInTriangle t pt then return pt else uniformTri t
