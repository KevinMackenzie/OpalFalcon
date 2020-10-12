module OpalFalcon.Geometry.Construction
  ( Construction (..),
    toObject,
  )
where

import qualified OpalFalcon.BaseTypes as Bt
import OpalFalcon.Math.Ray

data Construction
  = Union Construction Construction
  | Leaf Bt.Object

--   | Intersect Construction Construction
--   | Difference Construction Construction

toObject :: Construction -> Bt.Object
toObject c =
  Bt.MkObj
    { Bt.objPos = undefined,
      Bt.objIntersectRay = intersect c,
      Bt.objLightSource = Nothing
    }

-- Could be improved with BVH
intersect :: Construction -> Ray -> Maybe Bt.Hit
intersect (Leaf obj) ray = Bt.objIntersectRay obj ray
intersect (Union l r) ray@(Ray pos _)=
    let lh = intersect l ray
        rh = intersect r ray
     in Bt.closerHit pos lh rh
