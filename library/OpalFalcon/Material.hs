module OpalFalcon.Material where

import OpalFalcon.Math.Vector
import OpalFalcon.BaseTypes

-- TODO: this could be improved because we may have one material
-- that can accept multiple different reflection algorithms

-- Object with no reflectivity
mkDiffuseMat :: ColorRGBf -> AppliedMaterial
mkDiffuseMat dif = MkAppMat { matDiffuseColor = dif
                            , matApply = id
                            }

-- Simple material with a single reflective color
mkSimpleMat :: ColorRGBf -> ColorRGBf -> AppliedMaterial
mkSimpleMat dif refl = MkAppMat { matDiffuseColor = dif
                                , matApply = (|*|) refl
                                }

