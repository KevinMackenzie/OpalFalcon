module OpalFalcon.Scene
  ( ObjectCollection (..),
    Scene (..),
    lightSources,
    sampleLights,
  )
where

import Control.Monad.Random
import qualified OpalFalcon.BaseTypes as Bt
import qualified OpalFalcon.Math.Optics as R
import OpalFalcon.Scene.Camera (Camera)

class ObjectCollection c where

  probeCollection :: c -> R.Ray -> Maybe (Bt.Object, Bt.Hit)

  collectionLightSources :: c -> [Bt.LightSource]

data Scene o
  = MkScene
      { sceneObjects :: o,
        sceneCamera :: Camera
      }

lightSources :: (ObjectCollection c) => Scene c -> [Bt.LightSource]
lightSources = collectionLightSources . sceneObjects

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
