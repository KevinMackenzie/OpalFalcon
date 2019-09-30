module OpalFalcon.Material (
    mkDiffuseMat,
    mkSimpleMat
    ) where

import OpalFalcon.Math.Vector
import OpalFalcon.BaseTypes

-- TODO: this could be improved because we may have one material
-- that can accept multiple different reflection algorithms

-- BSSRDF materials should require an object collection of light sources for each step

-- TODO: LEFT HERE: Change materials so their functions aren't complied applied, which means they do get the hit position and incoming direction, since this might not be convenient to derive in functions which construct these materials (i.e. SphereMat)  Then, the BRDF can assume its in frame-ofo-reference to the normal.

-- Object with no reflectivity
mkDiffuseMat :: ColorRGBf -> Vec3d -> AppliedMaterial
mkDiffuseMat dif norm = 
    MkAppMat { matDiffuseColor = dif
             , brdf = \_ _ -> (1/pi) *| dif
             , importance = \_ -> id
             , emittence = \_ -> V3 0 0 0.5
             }

-- Simple material with a single reflective color
mkSimpleMat :: ColorRGBf -> ColorRGBf -> Vec3d -> AppliedMaterial
mkSimpleMat dif refl norm = 
    MkAppMat { matDiffuseColor = dif
             , brdf = \_ _ -> refl
             , importance = \inc _ -> reflect norm (negateVec inc)
             , emittence = \_ -> black
             }

