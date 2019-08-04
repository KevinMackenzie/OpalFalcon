module OpalFalcon.Material where

import OpalFalcon.Math.Vector
import OpalFalcon.Math.Ray
import OpalFalcon.BaseTypes

-- TODO: this could be improved because we may have one material
-- that can accept multiple different reflection algorithms

-- TODO: figure out how to handle materials, how they should mutate RTRays, etc.
-- When we have determined we should stop reflecting, we ray-test every light source and calc based on diffuse mat color
-- BSSRDF materials should require an object collection of light sources for each step

-- Object with no reflectivity
mkDiffuseMat :: ColorRGBf -> AppliedMaterial
mkDiffuseMat dif = MkAppMat { matDiffuseColor = dif
                            , matApply = \h r ->
                                    MkRtR { rayBase = MkRay (hitPos h) origin
                                          , rayColor = rayColor r
                                          , rayDepth = rayDepth r
                                          , rayHit = rayHit r
                                          }
                            }

reflTODO :: Hit -> Ray -> Ray
reflTODO h r@(MkRay rPos _) = MkRay (hitPos h) $ reflectRay r $ hitNorm h

-- Simple material with a single reflective color
mkSimpleMat :: ColorRGBf -> ColorRGBf -> AppliedMaterial
mkSimpleMat dif refl = MkAppMat { matDiffuseColor = dif
                                , matApply = \h r ->
                                    MkRtR { rayBase = reflTODO h $ rayBase r
                                          , rayColor = (rayColor r) |*| refl
                                          , rayDepth = rayDepth r
                                          , rayHit = rayHit r
                                          }
                                }

