{-# LANGUAGE RankNTypes #-}

module OpalFalcon.BaseTypes where

import Control.Monad.Random
import OpalFalcon.Math.Optics (Ray)
import OpalFalcon.Math.Vector
import qualified OpalFalcon.Util.Misc as Misc

-- These use the current position as the origin of the two vectors (so incoming vectors are "flipped")
--                        lInPos lInDir    lOutPos lOutDir    3 band color distrib
newtype Bssrdf = Bssrdf ((Vec3d, Vec3d) -> (Vec3d, Vec3d) -> ColorRGBf)

-- A BRDF with the "lOutDir" and Pos alrady applied
newtype RayBssrdf = RayBssrdf ((Vec3d, Vec3d) -> ColorRGBf)

--                            lInDir   lOutDir  3-band distribution
newtype PhaseFunc = PhaseFunc (Vec3d -> Vec3d -> ColorRGBf)

newtype Brdf = Brdf (Vec3d -> Vec3d -> ColorRGBf)

-- A BRDF with the "lOutDir" alrady applied
newtype RayBrdf = RayBrdf (Vec3d -> ColorRGBf)

mkRayBssrdf :: Bssrdf -> (Vec3d, Vec3d) -> RayBssrdf
mkRayBssrdf (Bssrdf bssrdf) rayInc = RayBssrdf $ flip bssrdf rayInc

mkRayBrdf :: Brdf -> Vec3d -> RayBrdf
mkRayBrdf (Brdf brdf) rayIncDir = RayBrdf $ flip brdf rayIncDir

mkBssrdf :: Brdf -> Bssrdf
mkBssrdf (Brdf brdf) = Bssrdf (\(_, iDir) (_, oDir) -> brdf iDir oDir)

mkBrdf :: Bssrdf -> Vec3d -> Brdf
mkBrdf (Bssrdf bssrdf) pos = Brdf $ (\i o -> bssrdf (pos, i) (pos, o))

-- I really don't like this
data HitCoords
  = HitNone
  | HitLocal Vec3d
  | HitIndexed Int HitCoords
  deriving (Show)

data Hit
  = MkHit
      { hitPos :: Vec3d,
        -- NOTE: Norms on interior hits also point outwards
        hitNorm :: Vec3d,
        hitInc :: Ray, -- The ray used to shoot this hit
        hitParam :: Double,
        -- I really don't like this
        hitCoords :: HitCoords
      }

hitLocal :: Hit -> Vec3d
hitLocal h = (\(HitLocal v) -> v) (hitCoords h)

hitIndexed :: Hit -> (Int, Vec3d)
hitIndexed h = (\(HitIndexed i (HitLocal v)) -> (i, v)) (hitCoords h)

flipHitNorm :: Hit -> Hit
flipHitNorm h =
  MkHit
    { hitPos = hitPos h,
      hitNorm = negateVec $ hitNorm h,
      hitInc = hitInc h,
      hitParam = hitParam h,
      hitCoords = hitCoords h
    }

instance Show Hit where
  show h = (foldl (\x -> (\y -> (++) x ((++) "\n    " y))) "Hit {" [(show $ hitPos h), (show $ hitNorm h), (show $ hitInc h), (show $ hitParam h), (show $ hitCoords h)]) ++ "\n}\n"

-- Object collects the minimum definition for an ray-tracable object
data Object
  = MkObj
      { objPos :: Vec3d,
        objIntersectRay :: Ray -> Maybe Hit,
        objHitMat :: Hit -> AppliedMaterial,
        objLightSource :: Maybe LightSource
      }

-- Hit types; eye is implied
data HitType = HDiff | HSpec | HLight

-- The ray path in reverse order (head is the previous bounce)
type Path = [HitType]

data RayTransmitResult
  = RayReflect Vec3d ColorRGBf -- Note: Reflectance needed here because BRDF of surfaces may have delta function
  | RayParticipate ParticipatingMaterial
  | RayScatter ScatteringMaterial
  | RayTerm

-- Photon shot from a light source
data EmissivePhoton = EPhoton !Ray !ColorRGBf deriving (Show)

-- TODO: Is refraction a special case?
data PhotonTransmitResult
  = PhotonReflect Vec3d ColorRGBf
  | PhotonScatter [EmissivePhoton] (Maybe EmissivePhoton) -- Scattering case returns photons to add to volume photon map, with output photon if not absorbed
  | PhotonStore Vec3d ColorRGBf
  | PhotonAbsorb

-- Note: all incoming directions must be "flipped"
data AppliedMaterial
  = AppliedMaterial
      { -- transmit a ray from a provided incoming direction; importance sampled.  Used in a path-tracing method
        transmitRay :: (forall g m. (Monad m, RandomGen g) => Vec3d -> RandT g m RayTransmitResult),
        -- Transmit a photon from a provided incoming direction; importance sampled.  Used in a russian-roulette method.  TODO: Add photon color to this instead of scaling after the fact
        transmitPhoton :: (forall g m. (Monad m, RandomGen g) => Vec3d -> RandT g m PhotonTransmitResult),
        -- TODO: Should this just be BRDF's and we use phase functions for subsurface scattering?
        surfaceBssrdf :: Bssrdf
      }

-- Represents a participating media bounded by a volume
data ParticipatingMaterial
  = ParticipatingMaterial
      { participateScatter :: Vec3d -> ColorRGBf,
        participateAbsorb :: Vec3d -> ColorRGBf,
        participatePhase :: PhaseFunc,
        -- Returns the importance-sampled out direction provided in an direction
        participateImportance :: (forall g m. (Monad m, RandomGen g) => Vec3d -> RandT g m Vec3d),
        participateExit :: Ray -> Maybe Hit -- Returns the Hit that would exit the medium.  Due to advancing ray by small 'epsilon', it may not still be in the material
      }

-- A subsurface scattering material is just a participating material
--  with refraction at the surface (plus optimizations)
data ScatteringMaterial
  = ScatteringMaterial
      { scatteringParticipate :: ParticipatingMaterial,
        -- Index of refraction
        scatteringRefract :: Double
      }

-- A function that maps a position in space with a power scale for lighting
newtype AttenuationFunc = AttenuationFunc (Vec3d -> Float)

-- An abstract representation for sampling a light
--                              pos    attenuation    color
data LightSample = LightSample Vec3d AttenuationFunc ColorRGBf

-- Provides function to sample light source from a point in the scene
data LightSource
  = MkLight
      { lightSample ::
          ( forall g m.
            (Monad m, RandomGen g) =>
            RandT g m [LightSample]
          ),
        emitPhotons ::
          ( forall g m.
            (Monad m, RandomGen g) =>
            Int ->
            RandT g m [EmissivePhoton]
          )
      }

closerHit :: Vec3d -> Hit -> Hit -> Bool
closerHit p h1 h2 =
  let f x = mag $ (hitPos x) |-| p
   in (f h1) < (f h2)

-- We don't really want to define `Ord` over Hits
closerHitM :: Vec3d -> Maybe Hit -> Maybe Hit -> Maybe Hit
closerHitM p mh1 mh2 =
  Misc.maybeCompare (closerHit p) mh1 mh2

tmap0 :: (a -> b) -> (a, c) -> (b, c)
tmap0 f (a, b) = (f a, b)
