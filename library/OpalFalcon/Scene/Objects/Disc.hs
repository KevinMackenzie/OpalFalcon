module OpalFalcon.Scene.Objects.Disc where

import OpalFalcon.BaseTypes
import OpalFalcon.Math.Ray
import OpalFalcon.Math.Vector
import OpalFalcon.Scene.Objects.Plane

-- A plane bounded to a circular region
data Disc = MkDisc Plane Double

hittestDisc :: Disc -> Ray -> Maybe Hit
hittestDisc q@(MkDisc p _) r = 
    case (hittestPlane p r) of
        Nothing -> Nothing
        Just h -> cropPlaneHit q h

cropPlaneHit :: Disc -> Hit -> Maybe Hit
cropPlaneHit (MkDisc (MkPlane (MkRay pPos _)) r) h = if (distance pPos $ hitPos h) < r then Just h else Nothing

