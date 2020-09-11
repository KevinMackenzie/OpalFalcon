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
mkVolumeMat :: (Ray -> Vec3d) -> (Vec3d -> ColorRGBf) -> (Vec3d -> ColorRGBf) -> Vec3d -> AppliedMaterial
mkVolumeMat probe absorb scatter enterPos =
  let extinct x = (absorb x) |+| (scatter x) -- Extinction coefficient
      albedo x = (scatter x) |/| (extinct x) -- scattering albedo
      advDist x xsi = (- (log xsi)) / (vecAvgComp $ extinct x) -- importance sampled distance between scattering events
      brdf _ _ = whitef |/ pi -- If we are using the brdf, its rendering diffuse
      phase _ _ = (1 / (4 * pi)) *| whitef
      -- TODO: This should be factored out
      traceScatterPhoton eph@(EPhoton r@(Ray prevPos dir) pow) =
        let exitPos = probe r
         in do
              (Ray eventPos _) <- (\xsi -> (advanceRay r $ float2Double (advDist prevPos xsi))) <$> getRandomNonZero
              if (exitPos |-| eventPos) |.| (exitPos |-| prevPos) < 0
                then-- The photon exited the volume before an event occured
                  return ([], Just $ EPhoton (Ray exitPos dir) pow)
                else do
                  rand0 <- getRandom -- Russian-Roulette the scattering/absorbing
                  if rand0 <= (vecAvgComp $ albedo eventPos)
                    then do
                      outDir <- getRandom -- importance sample phase function
                      (l, ex) <- traceScatterPhoton (EPhoton (Ray eventPos outDir) ((phase dir outDir) |*| pow))
                      return ((EPhoton (Ray eventPos dir) pow) : l, ex)
                    else return ([], Nothing)
      convResult (a, b) = PhotonScatter a b
   in AppliedMaterial
        { transmitRay = undefined,
          transmitPhoton = (\x -> convResult <$> traceScatterPhoton (EPhoton (Ray enterPos x) whitef)),
          photonBssrdf = mkBssrdf $ Brdf brdf
        }
