module OpalFalcon.Scene
  ( ObjectCollection (probeCollection),
    Scene (MkScene, objects, lightSources),
    sampleLights,
  )
where

import Control.Monad.Random
import qualified OpalFalcon.BaseTypes as Bt
import qualified OpalFalcon.Math.Ray as R

class ObjectCollection c where
  probeCollection :: c -> R.Ray -> Maybe Bt.Hit

data Scene o
  = MkScene
      { objects :: o,
        lightSources :: [Bt.LightSource]
      }

-- Samples all lights in the scene and returns a list of all samples that
--  are not occluded by other objects in the scene
sampleLights ::
  (Monad m, RandomGen g, ObjectCollection c) =>
  Scene c ->
  RandT g m [Bt.LightSample] -- List of point lights representing direct illumination
sampleLights scene =
  concat <$> (mapM Bt.lightSample $ lightSources scene)
-- sampleLights :: (ObjectCollection c) => ColorRGBf -> Scene c -> Vec3d -> ColorRGBf
-- sampleLights ambient scene pos =
--   foldr (|+|) ambient
--     $ map (\x -> (Bt.lightSample x) (probeCollection $ objects scene) pos)
--     $ lightSources scene
-- instance (ObjectCollection a) => Scene a where
--probeCollection s = probeCollection (objects s)
