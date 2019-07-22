{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module OpalFalcon.BaseTypes where

import OpalFalcon.Math.Vector
import OpalFalcon.Math.Ray

-- Type classes for reducing exposure on Objects
class Locatable a where
    position :: a -> Vec3d

-- The purpose of the intersection algorithm serves as a means to find the object it hit
--  but the purpose of the hit class is to cache intersection status and propagate the ray trace 
-- In each hit, we cache the material to bounce off of

-- We don't bother with material information at the top level since we let the objects
-- fully define how ray reflections modify the ray color

-- TODO: does this serve any real purpoes?
class Visible a where
    intersectRay :: a -> Ray -> Maybe Hit

class ObjectProvider a where
    getObject :: a -> Object

-- Primary Data types

-- Define ray with additional data beyond math ray
data RtRay = MkRtR { rayBase :: Ray
                   , rayColor :: ColorRGBf
                   , rayDepth :: Integer
                   -- , prevHit :: Hit
                   -- We could also bundle an RNG here for MC RT
                   }
-- We can take advantage of lazy evaluation so not all of these will evaluated every time
data Hit = MkHit { hitPos :: Vec3d
                 , hitNorm :: Vec3d
                 , hitInc :: Ray
                 , hitParam :: Double
                 , hitOut :: Vec3d
                 , hitMat :: AppliedMaterial
                 }

-- Object collects the minimum definition for an ray-tracable object
data Object = MkObj { objPos :: Vec3d
                    , objIntersectRay :: Ray -> Maybe Hit
                    }

data AppliedMaterial = MkAppMat { matDiffuseColor :: ColorRGBf
                                , matApply :: ColorRGBf -> ColorRGBf
                                }

-- Instances for Visible 
instance Visible Object where
    intersectRay = objIntersectRay

-- Instances for Locatable
instance Locatable Object where
    position = objPos
instance Locatable Ray where 
    position = pos
instance Locatable RtRay where
    position = pos . rayBase
instance Locatable Hit where
    position = hitPos


-- Constructors et al
defaultRtRay :: Ray -> RtRay
defaultRtRay base = 
    MkRtR { rayBase = base
          , rayColor = whitef
          , rayDepth = 0
          }

-- We don't really want to define `Ord` over Hits
closerHit :: Vec3d -> Maybe Hit -> Maybe Hit -> Maybe Hit
closerHit p mh1 mh2 = 
    case (mh1, mh2) of
        (Nothing, Nothing) -> Nothing
        (Nothing, Just v) -> Just v
        (Just v, Nothing) -> Just v
        (Just h1, Just h2) -> let f = \x -> mag $ (hitPos x) |-| p
                  in if (f h1) < (f h2) then Just h1 else Just h2


