module OpalFalcon.Photon.PhotonTracer where

import Control.Monad.Random
import OpalFalcon.BaseTypes
import OpalFalcon.Math.Ray
import OpalFalcon.Math.Vector
import OpalFalcon.Photon.Photon
import OpalFalcon.Scene
import System.Random

-- Photon shot from a light source
data EmissivePhoton = EPhoton !Ray !ColorRGBf

-- Traces a photon through the scene using russian-roulette based on BRDF for material
shootPhoton :: (Monad m, RandomGen g, ObjectCollection o) => Int -> Scene o -> EmissivePhoton -> RandT g m (Maybe Photon)
shootPhoton minBounces scene photon =
  let shoot depth ph@(EPhoton epr _) =
        case probeCollection (objects scene) epr of
          Nothing -> return Nothing
          Just h -> bounce depth ph h
      bounce depth (EPhoton (Ray _ prDir) pCol) MkHit {hitPos = pos, hitMat = m, hitNorm = norm} =
        let iDir = negateVec prDir
         in do
              (oDir, refl) <- transmitPhoton m iDir
              let reflAvg = vecAvgComp refl
               in do
                    rnum <- getRandom
                    if rnum < reflAvg
                      then shoot (depth + 1) (EPhoton (Ray pos oDir) (pCol |*| (refl |/ reflAvg)))
                      else
                        if minBounces > depth
                          then return Nothing
                          else return $ Just $ mkPhoton pos pCol iDir
   in shoot 0 photon
