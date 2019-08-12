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
                   , rayHit :: Maybe Hit
                   -- We could also bundle an RNG here for MC RT
                   }
    deriving (Show)
-- We can take advantage of lazy evaluation so not all of these will evaluated every time
data Hit = MkHit { hitPos :: Vec3d
                 , hitNorm :: Vec3d
                 , hitInc :: Ray
                 , hitParam :: Double
                 , hitMat :: AppliedMaterial
                 }

instance Show Hit where
    show h = (foldl (\x -> (\y -> (++) x ((++) "\n    " y))) "Hit {" [(show $ hitPos h), (show $ hitNorm h), (show $ hitInc h), (show $ hitParam h),  (show $ hitMat h)]) ++ "\n}\n"

-- Object collects the minimum definition for an ray-tracable object
data Object = MkObj { objPos :: Vec3d
                    , objIntersectRay :: Ray -> Maybe Hit
                    }

-- Provides function to sample light source from a point in the scene
--   This is only good for lambertian surfaces
data LightSource = MkLight { lightSample :: (Ray -> Maybe Hit) -> Vec3d -> ColorRGBf }

-- The concept is it is an arbitrary material applied to the parameter
--  of a hit for a specific object.  This lets us avoid having 
--  general-purpose "hit UV" properties and material mappings
data AppliedMaterial = MkAppMat { matDiffuseColor :: ColorRGBf
                                , matApply :: Hit -> RtRay -> RtRay
                                }
instance Show AppliedMaterial where
    show m = show $ matDiffuseColor m

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
          , rayHit = Nothing
          }

-- We don't really want to define `Ord` over Hits
closerHit :: Vec3d -> Maybe Hit -> Maybe Hit -> Maybe Hit
closerHit p mh1 mh2 = 
    let f x = mag $ (hitPos x) |-| p
        c x y = (f x) < (f y)
    in  maybeCompare c mh1 mh2

-- Where should this go?
maybeCompare :: (a -> a -> Bool) -> Maybe a -> Maybe a -> Maybe a
maybeCompare f m0 m1 =
    case (m0, m1) of
        (Nothing, Nothing) -> Nothing
        (Nothing, Just v) -> Just v
        (Just v, Nothing) -> Just v
        (Just h0, Just h1) -> if f h0 h1 then Just h0 else Just h1

