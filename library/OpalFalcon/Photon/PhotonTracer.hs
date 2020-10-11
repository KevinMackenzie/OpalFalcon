module OpalFalcon.Photon.PhotonTracer
  ( EmissivePhoton (..),
    shootPhoton,
    shootPhotons,
  )
where

import Control.Monad.Random
import Debug.Trace
import OpalFalcon.BaseTypes
import OpalFalcon.Math.Ray
import OpalFalcon.Math.Vector
import OpalFalcon.Photon.Photon
import OpalFalcon.Scene

ptEpsilon = 0.00001

-- Traces a photon through the scene using russian-roulette based on BRDF for material
shootPhoton :: (Monad m, RandomGen g, ObjectCollection o) => Int -> Scene o -> EmissivePhoton -> RandT g m ([Photon], [Photon])
shootPhoton minBounces scene photon =
  let shoot depth ph@(EPhoton epr _) =
        case probeCollection (sceneObjects scene) epr of
          Nothing -> return ([], [])
          Just h -> bounce depth ph h
      bounce depth (EPhoton (Ray _ prDir) pCol) MkHit {hitPos = hPos, hitMat = m, hitNorm = norm} =
        let iDir = negateVec prDir
            next (EPhoton (Ray oPos oDir) refl) =
              -- TODO: Since we use russian roulette, we don't want to modify the power of the photon during a bounce, but how do we reconcile this with a spectral model of reflectance and 3-band photons?  Using the method henrik suggests does NOT preserve power (increases) and using a power-preserving method looks wrong (white light in 3 bands --> 3x power in 1 band).  Not adjusting to preserve power makes it look too dark, however (as expected).
              -- ANSWER: This can be solved by shooting photons of a single wavelength and using a fully spectral model where the brdf has a wave-length parameter
              let oCol' = pCol |*| refl
                  oCol = oCol' |/ (vecAvgComp refl) --} |* ((vecCompSum pCol) / (vecCompSum oCol'))
                  res = shoot (depth + 1) (EPhoton (Ray (oPos |+| (ptEpsilon *| oDir)) oDir) oCol)
                  mO = vecCompSum oCol
                  mP = vecCompSum pCol
               in res -- if (abs $ mO - mP) > 0.0001*mP then trace ((show $ mP) ++ " --> " ++ (show $ mO)) res else res
         in do
              result <- transmitPhoton m iDir
              case result of
                PhotonAbsorb -> return ([], [])
                PhotonStore oDir refl ->
                  if minBounces > depth
                    then next (EPhoton (Ray hPos oDir) refl)
                    else (\(a, b) -> ((mkPhoton hPos pCol iDir) : a, b)) <$> next (EPhoton (Ray hPos oDir) refl)
                PhotonReflect oDir refl -> next (EPhoton (Ray hPos oDir) refl)
                PhotonScatter vPhotons outPhoton ->
                  -- Scale the volume photons by the source photon that was shot in
                  let volPhotons = map (\(EPhoton (Ray p d) pow) -> mkPhoton p (pow |*| pCol) d) vPhotons
                   in case outPhoton of -- If no photon leaves, then only add the volume photons
                        Nothing -> return ([], volPhotons)
                        -- Also remember to scale the emitted photon
                        Just (EPhoton r pow) -> (\(a, b) -> (a, volPhotons ++ b)) <$> next (EPhoton r (pow |*| pCol))
   in shoot 0 photon

-- TODO: This shoots a fixed number of photons per object, but we should
--  be targeting a total number of photons
shootPhotons :: (Monad m, RandomGen g, ObjectCollection c) => Int -> Scene c -> RandT g m ([Photon], [Photon])
shootPhotons minBounces scene =
  let emitters = lightSources scene
   in do
        photons <-
          mapM
            ( \x ->
                emitPhotons x 50000 >>= (mapM (shootPhoton minBounces scene))
            )
            emitters
        return $ (\(a, b) -> (concat a, concat b)) $ unzip $ concat photons
