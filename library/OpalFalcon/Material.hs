module OpalFalcon.Material (
    mkDiffuseMat,
    mkSimpleMat
    ) where

import OpalFalcon.Math.Vector
import OpalFalcon.Math.Ray
import OpalFalcon.BaseTypes

-- TODO: this could be improved because we may have one material
-- that can accept multiple different reflection algorithms

-- BSSRDF materials should require an object collection of light sources for each step

reflectRayOverHit :: Hit -> Ray -> Ray
reflectRayOverHit h r@(MkRay _ _) = MkRay (hitPos h) $ reflectRay r $ hitNorm h

-- Breaking constant accounts for "+1" that will follow this
breakConst :: Integer
breakConst = -2

diffuseApply :: Hit -> RtRay -> RtRay
diffuseApply h r = 
    MkRtR { rayBase = reflectRayOverHit h $ rayBase r
          , rayColor = rayColor r
          , rayDepth = breakConst
          , rayHit = rayHit r
          }

-- Object with no reflectivity
mkDiffuseMat :: ColorRGBf -> AppliedMaterial
mkDiffuseMat dif = 
    MkAppMat { matDiffuseColor = dif
             , matApply = diffuseApply
             }

-- Simple material with a single reflective color
mkSimpleMat :: ColorRGBf -> ColorRGBf -> AppliedMaterial
mkSimpleMat dif refl = 
    MkAppMat { matDiffuseColor = dif
             , matApply = \h r ->
                 MkRtR { rayBase = reflectRayOverHit h $ rayBase r
                       , rayColor = (rayColor r) |*| refl
                       , rayDepth = rayDepth r
                       , rayHit = rayHit r
                       }
             }

