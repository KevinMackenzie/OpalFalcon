module OpalFalcon.Scene (
    ObjectCollection(probeCollection),
    Scene(MkScene, objects, lightSources),
    sampleLights
    ) where

import qualified OpalFalcon.BaseTypes as Bt
import qualified OpalFalcon.Math.Ray as R
import OpalFalcon.Math.Vector

class ObjectCollection c where
    probeCollection :: c -> R.Ray -> Maybe Bt.Hit

data Scene o = MkScene { objects :: o
                       , lightSources :: [Bt.LightSource]
                       }

sampleLights :: (ObjectCollection c) => ColorRGBf -> Scene c -> Vec3d -> ColorRGBf
sampleLights a s p =
    foldr (|+|) a $ 
        map (\x -> (Bt.lightSample x) (probeCollection $ objects s) p) $ 
            lightSources s

-- instance (ObjectCollection a) => Scene a where
    --probeCollection s = probeCollection (objects s)

