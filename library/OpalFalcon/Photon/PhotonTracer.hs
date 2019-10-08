module OpalFalcon.Photon.PhotonTracer where

import OpalFalcon.Math.Ray
import OpalFalcon.Math.Vector
import OpalFalcon.Photon.Photon
import OpalFalcon.BaseTypes
import OpalFalcon.Scene

import System.Random

-- Photon shot from a light source
data EmissivePhoton = EPhoton !Ray !ColorRGBf

-- Uses russian roulette to shoot a photon through the scene
bouncePhoton :: (RandomGen g) => (g -> EmissivePhoton -> Maybe Photon) -> g -> EmissivePhoton -> Hit -> Maybe Photon
bouncePhoton shoot g (EPhoton (Ray _ prDir) pCol) hit
    | r < reflAvg = shoot g'' $ EPhoton (Ray hPos oDir) $ pCol |*| (refl |/ reflAvg)
    | otherwise   = Just $ mkPhoton hPos pCol iDir
    where m         = hitMat hit
          hPos      = hitPos hit
          iDir      = negateVec prDir -- Flipped incoming direction (points away from surface)
          (oDir, g')= tmap0 ((importance m iDir) . (clampHemisphere (hitNorm hit))) $ random g
          refl      = brdf m iDir oDir
          reflAvg   = vecAvgComp refl
          (r, g'')  = random g'


-- Traces a photon through the scene using russian-roulette based on BRDF for material
shootPhoton :: (RandomGen g, ObjectCollection o) => Scene o -> g -> EmissivePhoton -> Maybe Photon
shootPhoton scene g ph@(EPhoton r _) =  (probeCollection (objects scene) r) >>= (bouncePhoton (shootPhoton scene) g ph)
