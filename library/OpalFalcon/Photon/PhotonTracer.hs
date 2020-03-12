module OpalFalcon.Photon.PhotonTracer where

import Control.Monad.Random
import Debug.Trace
import OpalFalcon.BaseTypes
import OpalFalcon.Math.Ray
import OpalFalcon.Math.Vector
import OpalFalcon.Photon.Photon
import OpalFalcon.Scene
import System.Random

-- Photon shot from a light source
data EmissivePhoton = EPhoton !Ray !ColorRGBf

-- Traces a photon through the scene using russian-roulette based on BRDF for material
shootPhoton :: (Monad m, RandomGen g, ObjectCollection o) => Int -> Scene o -> EmissivePhoton -> RandT g m [Photon]
shootPhoton minBounces scene photon =
  let shoot depth ph@(EPhoton epr _) =
        case probeCollection (objects scene) epr of
          Nothing -> return []
          Just h -> bounce depth ph h
      bounce depth (EPhoton (Ray _ prDir) pCol) MkHit {hitPos = pos, hitMat = m, hitNorm = norm} =
        let iDir = negateVec prDir
            next oDir refl = shoot (depth + 1) (EPhoton (Ray pos oDir) (pCol |*| (refl |/ (vecAvgComp refl))))
         in do
              result <- transmitPhoton m iDir
              case result of
                PhotonAbsorb -> return []
                PhotonStore oDir refl ->
                  if minBounces > depth
                    then next oDir refl
                    else ((mkPhoton pos pCol iDir) :) <$> (next oDir refl)
                PhotonPass oDir refl -> next oDir refl
   in shoot 0 photon
