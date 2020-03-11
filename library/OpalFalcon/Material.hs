module OpalFalcon.Material
  ( mkDiffuseMat,
    mkSimpleMat,
  )
where

import Debug.Trace
import Control.Monad.Random
import OpalFalcon.BaseTypes
import OpalFalcon.Math.Vector

-- Material with purely diffuse reflectivity
mkDiffuseMat :: ColorRGBf -> Vec3d -> AppliedMaterial
mkDiffuseMat refl norm =
  mkSchlickMat
    SchlickMat
      { schlickSpecularReflectance = refl,
        schlickRoughness = 0.999,
        schlickAnisotropy = 0.001
      }
    norm
    (V3 0 0 1) -- If Anisotropy is small, the grain direction does not matter

-- Material with purely specular reflectivity
mkSimpleMat :: ColorRGBf -> Vec3d -> AppliedMaterial
mkSimpleMat refl norm =
  mkSchlickMat
    SchlickMat
      { schlickSpecularReflectance = refl,
        schlickRoughness = 0.001,
        schlickAnisotropy = 0.0001
      }
    norm
    (V3 0 0 1) -- If Anisotropy is small, the grain direction does not matter

data SchlickMaterial
  = SchlickMat
      { schlickSpecularReflectance :: ColorRGBf,
        schlickRoughness :: Double,
        schlickAnisotropy :: Double
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
      schlickAnisotropy = psi
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
      schlickAnisotropy = psi
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
        dTerm = ((gTerm v) * (gTerm v') * zTerm * aTerm) / (4 * pi * v * v') + aTerm * (1 - (gTerm v) * (gTerm v')) / pi
     in double2FloatVec $ sTerm |* (reflDiffuse / pi + reflGlossy * dTerm + reflSpecular * specBRDF)

-- This is my best guess, based on Henrick (2001) pg 26,
--  Referenced source [A Customizable Reflectance Model for Everyday Rendering (1993)]
--      and source [An Inexpensive BRDF Model for Physically‐based Rendering (1994)]
--  but there are some inconsistencies between them and errors in the original
-- Henrick only references the 1993 one, but uses some equations from the 1994 one, so idk
mkSchlickMat :: SchlickMaterial -> Vec3d -> Vec3d -> AppliedMaterial
mkSchlickMat
  mat@(SchlickMat {schlickRoughness = sigma})
  norm
  grainDir =
    let brdf = schlickBRDF mat norm grainDir
        (reflDiffuse, reflGlossy, reflSpecular) = schlickDeriveParameters sigma
        xmitRay iDir _ =
          do
            randVal <- getRandom -- Imporance sample the 3 brdf lobes
            if randVal < reflGlossy
              then do
                -- Glossy
                hVec <- schlickRandomGlossyHalfVec mat norm grainDir
                let oDir = reflect iDir hVec
                 in return $ RayPass oDir $ brdf False iDir oDir
              else
                if randVal < reflGlossy + reflDiffuse
                  then return $ RayTerm -- Diffuse
                  else
                    let oDir = reflect iDir norm
                     in return $ RayPass oDir $ brdf True iDir oDir
        xmitPhoton iDir =
          do
            randVal <- getRandom -- Imporance sample the 3 brdf lobes
            if randVal < reflGlossy
              then do
                -- Glossy
                hVec <- schlickRandomGlossyHalfVec mat norm grainDir
                let oDir = reflect iDir hVec
                 in return (oDir, brdf False iDir oDir)
              else
                if randVal < reflGlossy + reflDiffuse
                  then do
                    -- Note: This path-tracing solution only uses photon map for emittance
                    --    but it could also consider single diffuse bounces for direct illumination
                    oDir <- getRandom
                    return (oDir, brdf False iDir oDir)
                  else
                    let oDir = reflect iDir norm
                     in return (oDir, brdf True iDir oDir)
     in AppliedMaterial
          { transmitRay = xmitRay,
            transmitPhoton = xmitPhoton,
            -- TODO: do we want the glossy contributions in the radiance estimates?
            photonBrdf = brdf False -- \i o -> let c = brdf False i o in trace ((show i) ++ "'-,-'" ++ (show o) ++ "==>" ++ (show c)) c
          }
