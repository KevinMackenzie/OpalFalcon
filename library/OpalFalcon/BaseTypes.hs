{-# LANGUAGE RankNTypes #-}

module OpalFalcon.BaseTypes where

import Control.Monad.Random
import OpalFalcon.Math.Ray
import OpalFalcon.Math.Vector

-- These use the current position as the origin of the two vectors (so incoming vectors are "flipped")
--                        lInPos lInDir    lOutPos lOutDir    3 band color distrib
newtype Bssrdf = Bssrdf ((Vec3d, Vec3d) -> (Vec3d, Vec3d) -> ColorRGBf)

-- A BRDF with the "lOutDir" and Pos alrady applied
newtype RayBssrdf = RayBssrdf ((Vec3d, Vec3d) -> ColorRGBf)

--                lInDir   lOutDir  3-band distribution
newtype PhaseFunc = PhaseFunc (Vec3d -> Vec3d -> ColorRGBf)

newtype Brdf = Brdf (Vec3d -> Vec3d -> ColorRGBf)

-- A BRDF with the "lOutDir" alrady applied
newtype RayBrdf = RayBrdf (Vec3d -> ColorRGBf)

mkRayBrdf (Brdf brdf) rayIncDir = RayBrdf $ flip brdf rayIncDir

mkBssrdf (Brdf brdf) = Bssrdf (\(_, iDir) (_, oDir) -> brdf iDir oDir)
mkBrdf (Bssrdf bssrdf) pos = Brdf $ (\i o -> bssrdf (pos, i) (pos, o))

mkRayBssrdf (Bssrdf bssrdf) rayInc = RayBssrdf $ flip bssrdf rayInc

data Hit
  = MkHit
      { hitPos :: Vec3d,
        hitNorm :: Vec3d,
        hitInc :: Ray, -- The ray used to shoot this hit
        hitParam :: Double,
        hitMat :: AppliedMaterial
      }

instance Show Hit where
  show h = (foldl (\x -> (\y -> (++) x ((++) "\n    " y))) "Hit {" [(show $ hitPos h), (show $ hitNorm h), (show $ hitInc h), (show $ hitParam h)]) ++ "\n}\n"

-- Object collects the minimum definition for an ray-tracable object
data Object
  = MkObj
      { objPos :: Vec3d,
        objIntersectRay :: Ray -> Maybe Hit
      }

-- Hit types; eye is implied
data HitType = HDiff | HSpec | HLight

-- The ray path in reverse order (head is the previous bounce)
type Path = [HitType]

data RayTransmitResult
  = RayReflect Vec3d ColorRGBf
  | RayScatter Vec3d Vec3d ColorRGBf
  | RayTerm

-- TODO: Scattering
data PhotonTransmitResult
  = PhotonPass Vec3d ColorRGBf
  | PhotonStore Vec3d ColorRGBf
  | PhotonAbsorb

-- The concept is it is an arbitrary material applied to the parameter
--  of a hit for a specific object.  This lets us avoid having
--  general-purpose "hit UV" properties and material mappings
-- Note: all incoming directions must be "flipped"
-- TODO: investigate if it is possible to merge stochastic transmission functions
data AppliedMaterial
  = AppliedMaterial
      { -- Reflects a ray from a provided incoming direction.  Used in a path-tracing method
        transmitRay :: (forall g m. (Monad m, RandomGen g) => Vec3d -> ColorRGBf -> RandT g m RayTransmitResult),
        -- Reflects a photon from a provided incoming direction.  Used in a russian-roulette method
        transmitPhoton :: (forall g m. (Monad m, RandomGen g) => Vec3d -> RandT g m PhotonTransmitResult),
        -- The BSSRDF used when estimating irradiance from photon map
        photonBssrdf :: Bssrdf
      }

-- Provides function to sample light source from a point in the scene
--   This is only good for lambertian surfaces, so only BRDF's are supported
data LightSource
  = MkLight
      { lightSample ::
          ( forall g m.
            (Monad m, RandomGen g) =>
            (Ray -> Maybe Hit) -> -- hit func
            RayBrdf -> -- brdf with inc dir applied
            Vec3d -> -- position of sample
            RandT g m ColorRGBf -- Total outgoing radiance contribution from this light source
          )
      }

-- We don't really want to define `Ord` over Hits
closerHit :: Vec3d -> Maybe Hit -> Maybe Hit -> Maybe Hit
closerHit p mh1 mh2 =
  let f x = mag $ (hitPos x) |-| p
      c x y = (f x) < (f y)
   in maybeCompare c mh1 mh2

-- Where should this go?
maybeCompare :: (a -> a -> Bool) -> Maybe a -> Maybe a -> Maybe a
maybeCompare f m0 m1 =
  case (m0, m1) of
    (Nothing, Nothing) -> Nothing
    (Nothing, Just v) -> Just v
    (Just v, Nothing) -> Just v
    (Just h0, Just h1) -> if f h0 h1 then Just h0 else Just h1

tmap0 :: (a -> b) -> (a, c) -> (b, c)
tmap0 f (a, b) = (f a, b)
