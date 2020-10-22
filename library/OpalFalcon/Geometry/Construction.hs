module OpalFalcon.Geometry.Construction
  ( Construction (..),
    toObject,
  )
where

import qualified OpalFalcon.BaseTypes as Bt
import OpalFalcon.Math.Optics

data Construction
  = Union Construction Construction
  | Leaf Bt.Object

--   | Intersect Construction Construction
--   | Difference Construction Construction

toObject :: Construction -> Bt.Object
toObject c = undefined

-- Could be improved with BVH
intersect :: Construction -> Ray -> Maybe Bt.Hit
intersect (Leaf obj) ray = Bt.objIntersectRay obj ray
