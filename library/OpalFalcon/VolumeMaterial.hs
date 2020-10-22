module OpalFalcon.VolumeMaterial where

import Control.Monad.Random
import Debug.Trace
import GHC.Float (float2Double)
import OpalFalcon.BaseTypes
import OpalFalcon.Math.Optics
import OpalFalcon.Math.Vector
import OpalFalcon.Util.Random

traceScatterPhoton :: (Monad m, RandomGen g) => ParticipatingMaterial -> Integer -> EmissivePhoton -> RandT g m ([EmissivePhoton], Maybe (EmissivePhoton, Maybe Hit))
traceScatterPhoton
  mat@( ParticipatingMaterial
          { participateAbsorb = absorb,
            participateScatter = scatter,
            participateExit = probe,
            participateImportance = phaseSample
          }
        )
  gen
  eph@(EPhoton pRay@(Ray prevPos dir) pow) =
    let extinct x = (absorb x) |+| (scatter x) -- Extinction coefficient
        albedo x = (scatter x) |/| (extinct x) -- scattering albedo
        advDist x xsi = (- (log xsi)) / (vecAvgComp $ extinct x) -- importance sampled distance between scattering events
        randNextEvt r@(Ray x _) = (\xsi -> (advanceRay r $ float2Double (advDist x xsi))) <$> getRandomNonZero
     in case probe pRay of
          Nothing -> return ([], Just (eph, Nothing)) -- Thin geometry
          Just h@(MkHit {hitPos = exitPos}) ->
            do
              (Ray eventPos _) <- randNextEvt pRay
              if isBetween prevPos exitPos eventPos
                then-- The photon exited the volume before an event occured
                  return ([], Just (EPhoton (Ray exitPos dir) pow, Just h))
                else
                  let eventAlbedo = albedo eventPos
                      eventAlbedoAvg = vecAvgComp eventAlbedo
                   in do
                        rand0 <- getRandom -- Russian-Roulette the scattering/absorbing
                        if rand0 <= eventAlbedoAvg
                          then do
                            outDir <- phaseSample dir
                            (l, ex) <-
                              traceScatterPhoton
                                mat
                                (gen + 1)
                                ( EPhoton -- Since we're using russian roulete don't scale the power
                                    (Ray eventPos outDir)
                                    ((eventAlbedo |/ eventAlbedoAvg) |*| pow)
                                )
                            -- Don't accumulate single-scatter photons
                            return (if gen == 0 then l else (EPhoton (Ray eventPos dir) pow) : l, ex)
                          else return ([], Nothing)

shootScatterPhoton :: (Monad m, RandomGen g) => ParticipatingMaterial -> Vec3d -> UVec3d -> RandT g m ([EmissivePhoton], Maybe (EmissivePhoton, Maybe Hit))
shootScatterPhoton mat enterPos pDir =
  traceScatterPhoton mat 0 $
    EPhoton
      ( advanceRay
          (Ray enterPos $ inVec3 $ pDir)
          0.00001
      )
      whitef

-- Simple homogeneous isotropic participating medium
-- The 'probe' function returns a 'hit' where the ray would EXIT the medium, so it uses BACKFACE hits on primitives
mkVolumeMat :: ParticipatingMaterial -> Vec3d -> AppliedMaterial
mkVolumeMat mat enterPos =
  let brdf _ _ = whitef |/ pi -- If we are using the brdf, its rendering diffuse
      convResult (a, b) = PhotonScatter a (fst <$> b)
   in AppliedMaterial
        { transmitRay = (\_ -> return $ RayParticipate mat),
          transmitPhoton = (\x -> convResult <$> (shootScatterPhoton mat enterPos $ norm3 $ negateVec x)),
          surfaceBssrdf = mkBssrdf $ Brdf brdf
        }

mkScatteringMat :: ScatteringMaterial -> UVec3d -> Vec3d -> AppliedMaterial
mkScatteringMat
  mat@( ScatteringMaterial
          { scatteringParticipate = pMat,
            scatteringRefract = ior
          }
        )
  norm
  enterPos =
    let m = mkVolumeMat pMat enterPos
        refractPhoton (EPhoton r p, Just h) = (\x -> EPhoton x p) <$> (refractIRay (IRay r) (norm3 $ negateVec $ hitNorm h) ior 1.0)
        refractPhoton (eph, Nothing) = Just eph
        xmitPhoton x =
          case refract (norm3 x) norm 1.0 ior of
            Nothing -> return $ trace "Hmm" $ PhotonScatter [] $ Just $ EPhoton (Ray enterPos $ reflect x norm) whitef -- total internal reflection
            Just refr -> do
              (vphs, out) <- shootScatterPhoton pMat enterPos $ norm3 refr
              return $ PhotonScatter vphs (out >>= refractPhoton)
     in AppliedMaterial
          { transmitRay = (\_ -> return $ RayScatter mat),
            transmitPhoton = xmitPhoton,
            surfaceBssrdf = mkBssrdf $ Brdf (\_ _ -> whitef |/ pi)
          }
