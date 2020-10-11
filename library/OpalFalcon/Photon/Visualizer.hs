module OpalFalcon.Photon.Visualizer where

import Debug.Trace

import Control.Monad.ST
import Data.Array
import Data.Array.ST
import Data.Maybe
import OpalFalcon.BaseTypes
import OpalFalcon.Math.Ray
import OpalFalcon.Math.Transformations
import OpalFalcon.Math.Vector
import OpalFalcon.Photon.Photon
import OpalFalcon.XTracing.RayTraceUtils
import OpalFalcon.Scene
import OpalFalcon.Scene.Camera

-- This will project the points into camera-space, flatten to a 2D plane, and then sample each photon for each pixel using a gaussian kernel

-- Renders a list of photons as points and uses the scene to occlude photons from the camera
renderPhotons' :: (ObjectCollection o) => Scene o -> Camera -> Integer -> [Photon] -> [Photon]
renderPhotons' sc cam px phs =
  let preCull =
        applyTransformStack
          [ cameraProjTransform cam,
            cameraViewTransform cam
          ]
      postCull =
        applyTransformStack
          [ cameraPixelTransform cam px,
            orthoZ
          ]
      tf s (Photon pPos pPow pDir pFlags) =
        let nPos = fromHomo $ s $ toHomoPos pPos
            nDir = fromHomoDir $ s $ toHomoDir pDir
         in Photon nPos pPow nDir pFlags
      phs' = filter (occludePhoton sc (cameraPos cam)) phs
   in map (tf postCull) $ filter (cullPhoton cam) $ map (tf preCull) phs'

-- TODO: Something about this is broken (the camera is too zoomed in or something)
renderPhotons :: (ObjectCollection o) => Scene o -> Camera -> Integer -> [Photon] -> FrameBuffer
renderPhotons sc cam px phs = rasterPhotons (cameraFrameBlank cam px) $ renderPhotons' sc cam px phs

-- Returns True if there is an unobstructed path from the photon to a point
occludePhoton :: (ObjectCollection o) => Scene o -> Vec3d -> Photon -> Bool
occludePhoton sc cp (Photon p _ _ _) = isNothing $ probeCollection (sceneObjects sc) (Ray p (normalize (cp |-| p)))

-- Returns True if the photon is in view of the camera
cullPhoton :: Camera -> Photon -> Bool
cullPhoton cam (Photon pos _ _ _) = cameraClipPoint cam pos

-- Rasters photon in pixel-space to output pixels
-- TODO: this only affects the pixel the photon is it.  It should use the gaussian
rasterPhoton :: Int -> (STArray s Int ColorRGBf) -> Photon -> ST s ()
rasterPhoton w pxs (Photon (V3 x y _) pPow _ _) =
  let idx = ((\(V2 x' y') -> (y' * w + x')) $ fmap (fromInteger . floor) (V2 x y))
   in do curr <- readArray pxs idx; writeArray pxs idx (pPow |+| curr)

toSTArray :: (Ix i) => Array i e -> ST s (STArray s i e)
toSTArray arr = newListArray (bounds arr) (elems arr)

rasterPhotons' :: Int -> Array Int ColorRGBf -> [Photon] -> ST s (STArray s Int ColorRGBf)
rasterPhotons' w frame phs =
  do
    pxs <- toSTArray frame
    _ <- mapM (rasterPhoton w pxs) phs
    return pxs

rasterPhotons :: FrameBuffer -> [Photon] -> FrameBuffer
rasterPhotons frame phs =
  FB
    { fbWidth = fbWidth frame,
      fbHeight = fbHeight frame,
      fbData = runSTArray $ rasterPhotons' (fbWidth frame) (fbData frame) phs
    }

renderIlluminance :: (ObjectCollection o) => SurfaceRadiance -> Scene o -> Camera -> Int -> [ColorRGBf]
renderIlluminance glob scene cam height =
  let shoot r =
        case probeCollection (sceneObjects scene) r of
          Nothing -> black
          Just h -> {-trace (show c)-} c
            where
              c = glob (hitPos h) (negateVec incDir) (hitNorm h) (surfaceBssrdf $ hitMat h)
              (Ray _ incDir) = hitInc h
   in map shoot $ genRays cam height
