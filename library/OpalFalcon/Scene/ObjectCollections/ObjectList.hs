module OpalFalcon.Scene.ObjectCollections.ObjectList (
    ObjectList(MkObjList, objList)
    ) where

import OpalFalcon.Math.Ray
import OpalFalcon.BaseTypes
import OpalFalcon.Scene

data ObjectList = MkObjList { objList :: [Object] }

instance ObjectCollection ObjectList where
    probeCollection ol r@(Ray pos _) = foldr (closerHit pos) Nothing $ map (\x -> objIntersectRay x r) $ objList ol

