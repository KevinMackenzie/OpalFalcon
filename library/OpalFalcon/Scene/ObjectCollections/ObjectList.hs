module OpalFalcon.Scene.ObjectCollections.ObjectList
  ( ObjectList (..),
  )
where

import OpalFalcon.BaseTypes
import OpalFalcon.Math.Ray
import OpalFalcon.Scene
import OpalFalcon.Util.Misc (filterJust)

data ObjectList = MkObjList {objList :: [Object]}

instance ObjectCollection ObjectList where

  probeCollection ol r@(Ray pos _) = foldr (closerHit pos) Nothing $ map (\x -> objIntersectRay x r) $ objList ol

  collectionLightSources (MkObjList {objList = ol}) = filterJust $ map objLightSource ol
