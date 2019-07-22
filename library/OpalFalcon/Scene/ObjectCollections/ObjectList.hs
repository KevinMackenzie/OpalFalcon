module OpalFalcon.Scene.ObjectCollections.ObjectList where

import OpalFalcon.Math.Ray
import OpalFalcon.BaseTypes
import OpalFalcon.Scene

data ObjectList = MkObjList { objList :: [Object] }

instance ObjectCollection ObjectList where
    probeCollection ol r = foldr (closerHit $ pos r) Nothing $ map (\x -> objIntersectRay x r) $ objList ol

