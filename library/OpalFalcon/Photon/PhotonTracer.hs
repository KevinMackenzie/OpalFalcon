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

-- Uses russian roulette to shoot a photon through the scene
-- bouncePhoton :: (Monad m, RandomGen g) => Int -> Int -> (EmissivePhoton -> RandR g m (Maybe Photon)) -> g -> EmissivePhoton -> Hit -> RandT g m (Maybe Photon)
-- bouncePhoton minBounces depth shoot g (EPhoton (Ray _ prDir) pCol) hit
--   | r < reflAvg = shoot g'' $ EPhoton (Ray hPos oDir) $ pCol |*| (refl |/ reflAvg)
--   | minBounces > depth = return Nothing
--   | otherwise = return $ Just $ mkPhoton hPos pCol iDir
--   where
--     m = hitMat hit
--     hPos = hitPos hit
--     iDir = negateVec prDir
--     (oDir, g') = tmap0 ((importance m iDir) . (clampHemisphere (hitNorm hit))) $ random g
--     refl = brdf m iDir oDir
--     reflAvg = vecAvgComp refl
--     (r, g'') = random g'

-- Traces a photon through the scene using russian-roulette based on BRDF for material
shootPhoton :: (Monad m, RandomGen g, ObjectCollection o) => Int -> Scene o -> EmissivePhoton -> RandT g m (Maybe Photon)
shootPhoton minBounces scene photon =
  let shoot depth ph@(EPhoton epr _) =
        case probeCollection (objects scene) epr of
          Nothing -> return Nothing
          Just ht -> bounce depth ph ht
      bounce depth (EPhoton (Ray _ prDir) pCol) (MkHit {hitPos = pos, hitMat = m, hitNorm = norm}) =
        do
          iDir <- return $ negateVec prDir
          oDir <- ((importance m iDir) . (clampHemisphere norm)) <$> getRandom
          refl <- return $ brdf m iDir oDir
          reflAvg <- return $ vecAvgComp refl
          rnum <- getRandom
          if (rnum < reflAvg)
            then shoot (depth + 1) (EPhoton (Ray pos oDir) (pCol |*| (refl |/ reflAvg)))
            else
              if minBounces > depth
                then return Nothing
                else return $ Just $ mkPhoton pos pCol iDir
   in shoot 0 photon
