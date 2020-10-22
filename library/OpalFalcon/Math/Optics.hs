module OpalFalcon.Math.Optics
  ( Ray (..),
    IRay (..),
    ORay (..),
    rayPos,
    rayDir,
    pointAtParameter,
    advanceRay,
    advanceORay,
    reflectRay,
    reflect,
    refract,
    refractIRay,
    fromIRay,
    fromORay,
    flipIRay,
    flipORay,
  )
where

import Debug.Trace
import OpalFalcon.Math.Vector

data Ray = Ray Vec3d Vec3d
  deriving (Eq, Show, Read)

newtype IRay = IRay Ray deriving (Eq, Show, Read)

newtype ORay = ORay Ray deriving (Eq, Show, Read)

fromIRay :: IRay -> Ray
fromIRay (IRay r) = r

fromORay :: ORay -> Ray
fromORay (ORay r) = r

flipIRay :: IRay -> ORay
flipIRay (IRay (Ray p d)) = ORay $ Ray p $ negateVec d

flipORay :: ORay -> IRay
flipORay (ORay (Ray p d)) = IRay $ Ray p $ negateVec d

rayPos :: Ray -> Vec3d
rayPos (Ray p _) = p

rayDir :: Ray -> Vec3d
rayDir (Ray _ d) = d

{-# INLINE pointAtParameter #-}
pointAtParameter :: Ray -> Double -> Vec3d
pointAtParameter (Ray rPos rDir) p = rPos |+| (p *| rDir)

{-# INLINE advanceRay #-}
advanceRay :: Ray -> Double -> Ray
advanceRay r@(Ray _ d) p = Ray (pointAtParameter r p) d

{-# INLINE advanceORay #-}
advanceORay :: ORay -> Double -> ORay
advanceORay (ORay r@(Ray _ d)) p = ORay $ Ray (pointAtParameter r p) d

{-# INLINE reflectRay #-}
reflectRay :: Ray -> UVec3d -> Vec3d
reflectRay (Ray _ d) norm = reflect (negateVec d) norm

{-# INLINE reflect #-}
reflect :: (Fractional a) => Vec3 a -> UVec3 a -> Vec3 a
reflect toSrc norm = ((2 * (toSrc |.| n)) *| n) |-| toSrc where n = inVec3 norm

{-# INLINE refractIRay #-}
refractIRay :: IRay -> UVec3d -> Double -> Double -> Maybe Ray
refractIRay (IRay (Ray p d)) norm nFrom nTo = (Ray p) <$> (refract (norm3 $ negateVec d) norm nFrom nTo)

{-# INLINE refract #-}
refract :: UVec3d -> UVec3d -> Double -> Double -> Maybe Vec3d
refract toSrcU normU nFrom nTo =
  let toSrc = inVec3 toSrcU
      norm = inVec3 normU
      n1n2 = nFrom / nTo
      cosTh1 = toSrc |.| norm
      cosTh22 = 1 - (n1n2 ^ 2) * (1 - cosTh1 ^ 2)
      cosTh2 = sqrt $ cosTh22
   in if cosTh1 < 0
        then traceShow cosTh1 undefined
        else if cosTh22 < 0
            then Nothing
            else Just $ ((n1n2 * cosTh1 - cosTh2) *| norm) |-| (n1n2 *| toSrc)
