module OpalFalcon.Photon.PhotonTracer (EmissivePhoton (..), shootPhoton) where

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

ptEpsilon = 0.00001

-- Traces a photon through the scene using russian-roulette based on BRDF for material
shootPhoton :: (Monad m, RandomGen g, ObjectCollection o) => Int -> Scene o -> EmissivePhoton -> RandT g m [Photon]
shootPhoton minBounces scene photon =
  let shoot depth ph@(EPhoton epr _) =
        case probeCollection (objects scene) epr of
          Nothing -> return []
          Just h -> bounce depth ph h
      bounce depth (EPhoton (Ray _ prDir) pCol) MkHit {hitPos = pos, hitMat = m, hitNorm = norm} =
        let iDir = negateVec prDir
            next oDir refl =
              -- TODO: Since we use russian roulette, we don't want to modify the power of the photon during a bounce, but how do we reconcile this with a spectral model of reflectance and 3-band photons?  Using the method henrik suggests does NOT preserve power (increases) and using a power-preserving method looks wrong (white light in 3 bands --> 3x power in 1 band).  Not adjusting to preserve power makes it look too dark, however (as expected).
              let oCol' = pCol |*| refl
                  oCol = oCol' |/ (vecAvgComp refl) --} |* ((vecCompSum pCol) / (vecCompSum oCol'))
                  res = shoot (depth + 1) (EPhoton (Ray (pos |+| (ptEpsilon *| oDir)) oDir) oCol)
                  mO = vecCompSum oCol
                  mP = vecCompSum pCol
               in res -- if (abs $ mO - mP) > 0.0001*mP then trace ((show $ mP) ++ " --> " ++ (show $ mO)) res else res
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
