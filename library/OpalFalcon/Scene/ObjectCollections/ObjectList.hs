module OpalFalcon.Scene.ObjectCollections.ObjectList
  ( ObjectList (..),
  )
where

import OpalFalcon.BaseTypes
import OpalFalcon.Math.Optics
import OpalFalcon.Scene
import OpalFalcon.Util.Misc (filterJust, maybeCompare, zipF)

data ObjectList = MkObjList {objList :: [Object]}

instance ObjectCollection ObjectList where

  probeCollection = probeList

  collectionLightSources (MkObjList {objList = ol}) = filterJust $ map objLightSource ol

probeList :: ObjectList -> Ray -> Maybe (Object, Hit)
probeList ol r@(Ray pos _) =
  let hits = zipF (objList ol) $ map (\x -> objIntersectRay x r) $ objList ol
      accum (_, h1) (_, h2) = closerHit pos h1 h2
   in foldl (maybeCompare accum) Nothing hits
