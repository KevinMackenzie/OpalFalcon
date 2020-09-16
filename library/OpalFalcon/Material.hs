module OpalFalcon.Material
  ( mkDiffuseMat,
    mkSpecularMat,
    mkDiffuseMatSchlick,
    mkSpecularMatSchlick,
  )
where

import Control.Monad.Random
import GHC.Float (double2Float, float2Double)
import OpalFalcon.BaseTypes
import OpalFalcon.Math.Lighting (cosWeightedDir)
import OpalFalcon.Math.Vector

-- Material with purely diffuse reflectivity
mkDiffuseMat :: ColorRGBf -> Vec3d -> AppliedMaterial
mkDiffuseMat refl norm =
  let brdf _ _ = refl |/ pi
      xmitRay _ = return RayTerm
      xmitPhoton iDir = do
        rrRand <- getRandom -- Russian-Roulette the surface reflectance
        if rrRand >= (vecAvgComp refl)
          then return PhotonAbsorb
          else do
            oDir <- cosWeightedDir norm
            return $ PhotonStore oDir refl
   in AppliedMaterial
        { transmitRay = xmitRay,
          transmitPhoton = xmitPhoton,
          surfaceBssrdf = mkBssrdf $ Brdf brdf
        }

mkDiffuseMatSchlick :: ColorRGBf -> Vec3d -> AppliedMaterial
mkDiffuseMatSchlick refl norm =
  mkSchlickMat
    SchlickMat
      { schlickSpecularReflectance = refl,
        schlickRoughness = 0.999999,
        schlickIsotropy = 1.0
      }
    norm
    (V3 0 0 1) -- If the material is isotropic, the grain direction does not matter

mkSpecularMat :: ColorRGBf -> Vec3d -> AppliedMaterial
mkSpecularMat refl norm =
  let xmitRay iDir = return $ RayReflect (reflect iDir norm) refl
      xmitPhoton iDir = return $ PhotonReflect (reflect iDir norm) refl
   in AppliedMaterial
        { transmitRay = xmitRay,
          transmitPhoton = xmitPhoton,
          surfaceBssrdf = mkBssrdf $ Brdf (\_ _ -> black)
        }

-- Material with purely specular reflectivity
mkSpecularMatSchlick :: ColorRGBf -> Vec3d -> AppliedMaterial
mkSpecularMatSchlick refl norm =
  mkSchlickMat
    SchlickMat
      { schlickSpecularReflectance = refl,
        schlickRoughness = 0.001,
        schlickIsotropy = 1.0
      }
    norm
    (V3 0 0 1) -- If the material is isotropic, the grain direction does not matter

data SchlickMaterial
  = SchlickMat
      { schlickSpecularReflectance :: ColorRGBf,
        schlickRoughness :: Double,
        schlickIsotropy :: Double
      }

swapIf :: Bool -> (a, a) -> (a, a)
swapIf v (x, y) = if v then (y, x) else (x, y)

schlickDeriveParameters :: Double -> (Double, Double, Double)
schlickDeriveParameters sigma =
  let reflGlossy = 4 * sigma * (1 - sigma)
      (reflDiffuse, reflSpecular) = swapIf (sigma >= 0.5) (0, 1 - reflGlossy)
   in (reflDiffuse, reflGlossy, reflSpecular)

schlickRandomGlossyHalfVec :: (Monad m, RandomGen g) => SchlickMaterial -> Vec3d -> Vec3d -> RandT g m Vec3d
schlickRandomGlossyHalfVec
  SchlickMat
    { schlickSpecularReflectance = f0,
      schlickRoughness = sigma,
      schlickIsotropy = psi
    }
  norm
  grainDir =
    do
      r0 <- getRandom
      r1 <- getRandom
      let t = sqrt $ r0 / (sigma - r0 * sigma + r0)
          w = cos $ (pi / 2) * (sqrt $ psi ^ 2 * r1 ^ 2 / (1 - r1 ^ 2 + r1 ^ 2 * psi ^ 2))
          -- I _think_ this is correct
          hVec = normalize $ (norm |* t) |+| (grainDir |* w)
       in return hVec

schlickBRDF :: SchlickMaterial -> Vec3d -> Vec3d -> Bool -> Vec3d -> Vec3d -> ColorRGBf
schlickBRDF
  SchlickMat
    { schlickSpecularReflectance = f0,
      schlickRoughness = sigma,
      schlickIsotropy = psi
    }
  norm
  grainDir
  isSpec
  iDir
  oDir =
    let f0HighP = float2DoubleVec f0
        (reflDiffuse, reflGlossy, reflSpecular) = schlickDeriveParameters sigma
        hVec = normalize $ iDir |+| oDir
        u = iDir |.| hVec
        t = norm |.| hVec
        v = iDir |.| norm
        v' = oDir |.| norm
        w = grainDir |.| (normalize $ hVec |-| ((norm |.| hVec) *| norm))
        specBRDF = if isSpec then vecAvgComp f0HighP else 0 -- todo correct?
        sTerm = f0HighP |+| ((whitef |-| f0HighP) |* ((1 - u) ^ 5))
        gTerm x = x / (sigma - sigma * x + x)
        zTerm = sigma / ((1 + sigma * t ^ 2 - t ^ 2) ^ 2)
        aTerm = sqrt $ psi / (psi ^ 2 - psi ^ 2 * w ^ 2 + w ^ 2)
        -- The "D" term differs between the two papers
        dTerm =
          ( if v == 0 || v' == 0
              then 0 -- TODO: This is because we don't/can't assume general position
              else ((gTerm v) * (gTerm v') * zTerm * aTerm) / (4 * pi * v * v')
          )
            + aTerm
            * (1 - (gTerm v) * (gTerm v'))
            / pi
     in double2FloatVec $ sTerm |* (reflDiffuse / pi + reflGlossy * dTerm + reflSpecular * specBRDF)

-- This is my best guess, based on Henrick (2001) pg 26,
--  Referenced source [A Customizable Reflectance Model for Everyday Rendering (1993)]
--      and source [An Inexpensive BRDF Model for Physically‐based Rendering (1994)]
--  but there are some inconsistencies between them and errors in the original
-- Henrick only references the 1993 one, but uses some equations from the 1994 one, so idk
mkSchlickMat :: SchlickMaterial -> Vec3d -> Vec3d -> AppliedMaterial
mkSchlickMat
  mat@(SchlickMat {schlickSpecularReflectance = f0, schlickRoughness = sigma})
  norm
  grainDir =
    let brdf = schlickBRDF mat norm grainDir
        (reflDiffuse, reflGlossy, _) = schlickDeriveParameters sigma
        xmitRay iDir = do
          randVal <- getRandom -- Imporance sample the 3 brdf lobes
          if randVal < reflGlossy
            then do
              -- Glossy
              hVec <- schlickRandomGlossyHalfVec mat norm grainDir
              let oDir = reflect iDir hVec
               in return $ RayReflect oDir $ brdf False iDir oDir
            else
              if randVal < reflGlossy + reflDiffuse
                then return $ RayTerm -- Diffuse
                else
                  let oDir = reflect iDir norm
                   in return $ RayReflect oDir $ brdf True iDir oDir
        -- Since all reflections are specular, the "reflectance" we return back is just the specular reflectance
        -- TODO: We may have to compensate for the geometry term for glossy reflections
        xmitPhoton iDir = do
          rrRand <- getRandom -- Russian-Roulette the surface reflectance
          if rrRand >= (vecAvgComp f0)
            then return PhotonAbsorb
            else do
              randVal <- getRandom -- Imporance sample the 3 brdf lobes
              if randVal < reflGlossy
                then do
                  -- Glossy
                  hVec <- schlickRandomGlossyHalfVec mat norm grainDir
                  let oDir = reflect iDir hVec
                   in return $ PhotonReflect oDir f0
                else
                  if randVal < reflGlossy + reflDiffuse
                    then do
                      -- Diffuse:
                      oDir <- cosWeightedDir norm
                      return $ PhotonStore oDir f0
                    else
                      let oDir = reflect iDir norm -- Specular
                       in return $ PhotonReflect oDir f0
     in AppliedMaterial
          { transmitRay = xmitRay,
            transmitPhoton = xmitPhoton,
            surfaceBssrdf = mkBssrdf $ Brdf $ brdf False -- \i o -> let c = brdf False i o in trace ((show i) ++ "'-,-'" ++ (show o) ++ "==>" ++ (show c)) c
          }
