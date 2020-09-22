module OpalFalcon.VolumeMaterial where

import Control.Monad.Random
import GHC.Float (double2Float, float2Double)
import OpalFalcon.BaseTypes
import OpalFalcon.Math.Lighting (cosWeightedDir)
import OpalFalcon.Math.Ray
import OpalFalcon.Math.Vector
import OpalFalcon.Util.Random

-- Simple homogeneous isotropic participating medium
-- The 'probe' function returns a 'hit' where the ray would EXIT the medium, so it uses BACKFACE hits on primitives
mkVolumeMat :: ParticipatingMaterial -> Vec3d -> AppliedMaterial
mkVolumeMat mat@(ParticipatingMaterial {participateAbsorb = absorb, participateScatter = scatter, participatePhase = (PhaseFunc phase), participateExit = probe}) enterPos =
  let extinct x = (absorb x) |+| (scatter x) -- Extinction coefficient
      albedo x = (scatter x) |/| (extinct x) -- scattering albedo
      advDist x xsi = (- (log xsi)) / (vecAvgComp $ extinct x) -- importance sampled distance between scattering events
      randNextEvt r@(Ray x _) = (\xsi -> (advanceRay r $ float2Double (advDist x xsi))) <$> getRandomNonZero
      brdf _ _ = whitef |/ pi -- If we are using the brdf, its rendering diffuse
          -- TODO: This should be factored out
      traceScatterPhoton gen eph@(EPhoton r@(Ray prevPos dir) pow) =
        let exitPos = probe r
         in do
              (Ray eventPos _) <- randNextEvt r
              if isBetween prevPos exitPos eventPos
                then-- The photon exited the volume before an event occured
                  return ([], Just $ EPhoton (Ray exitPos dir) pow)
                else
                  let eventAlbedo = albedo eventPos
                      eventAlbedoAvg = vecAvgComp eventAlbedo
                   in do
                        rand0 <- getRandom -- Russian-Roulette the scattering/absorbing
                        if rand0 <= eventAlbedoAvg
                          then do
                            outDir <- getRandom -- importance sample phase function
                            (l, ex) <-
                              traceScatterPhoton
                                (gen + 1)
                                ( EPhoton -- Since we're using russian roulete don't scale the power
                                    (Ray eventPos outDir)
                                    ((eventAlbedo |/ eventAlbedoAvg) |*| pow)
                                )
                            -- Don't accumulate single-scatter photons
                            return (if gen == 0 then l else (EPhoton (Ray eventPos dir) pow) : l, ex)
                          else return ([], Nothing)
      convResult (a, b) = PhotonScatter a b
   in AppliedMaterial
        { transmitRay = (\_ -> return $ RayParticipate mat),
          transmitPhoton = (\x -> convResult <$> (traceScatterPhoton 0 (EPhoton (advanceRay (Ray enterPos (negateVec x)) 0.00001) whitef))),
          surfaceBssrdf = mkBssrdf $ Brdf brdf
        }
