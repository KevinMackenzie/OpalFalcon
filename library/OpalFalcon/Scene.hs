module OpalFalcon.Scene
  ( ObjectCollection (probeCollection),
    Scene (MkScene, objects, lightSources),
    sampleLights,
  )
where

import Control.Monad (foldM)
import Control.Monad.Random
import qualified OpalFalcon.BaseTypes as Bt
import qualified OpalFalcon.Math.Ray as R
import OpalFalcon.Math.Vector
import System.Random

class ObjectCollection c where
  probeCollection :: c -> R.Ray -> Maybe Bt.Hit

data Scene o
  = MkScene
      { objects :: o,
        lightSources :: [Bt.LightSource]
      }

sampleLights ::
  (Monad m, RandomGen g, ObjectCollection c) =>
  Scene c ->
  Bt.RayBrdf ->
  Vec3d -> -- position of sample
  RandT g m ColorRGBf -- Total outgoing radiance contribution from direct illumination
sampleLights scene brdf pos =
  do
    contribs <- mapM (\l -> (Bt.lightSample l) (probeCollection $ objects scene) brdf pos) $ lightSources scene
    foldM (\x y -> return $ x |+| y) black contribs
-- sampleLights :: (ObjectCollection c) => ColorRGBf -> Scene c -> Vec3d -> ColorRGBf
-- sampleLights ambient scene pos =
--   foldr (|+|) ambient
--     $ map (\x -> (Bt.lightSample x) (probeCollection $ objects scene) pos)
--     $ lightSources scene
-- instance (ObjectCollection a) => Scene a where
--probeCollection s = probeCollection (objects s)
