module OpalFalcon.Photon.Visualizer where

import Data.Array
import Data.Maybe
import OpalFalcon.BaseTypes
import OpalFalcon.Math.Lighting
import OpalFalcon.Math.Ray
import OpalFalcon.Math.Transformations
import OpalFalcon.Math.Vector
import OpalFalcon.Photon.Photon
import OpalFalcon.Scene
import OpalFalcon.Scene.Camera

-- This will project the points into camera-space, flatten to a 2D plane, and then sample each photon for each pixel using a gaussian kernel

-- Renders a list of photons as points and uses the scene to occlude photons from the camera
renderPhotons' :: (ObjectCollection o) => Scene o -> Camera -> Integer -> [Photon] -> [Photon]
renderPhotons' sc cam px phs =
  let s =
        applyTransformStack
          [ cameraViewTransform cam,
            cameraProjTransform cam,
            orthoZ,
            cameraPixelTransform cam px
          ]
      tf (Photon pPos pPow pDir pFlags) =
        let nPos = fromHomo $ s $ toHomoPos pPos
            nDir = fromHomoDir $ s $ toHomoDir pDir
            nPow = double2FloatVec $ attenuateVec (float2DoubleVec pPow) pPos $ cameraPos cam
         in Photon nPos nPow nDir pFlags
      phs' = filter (cullPhoton cam) $ filter (occludePhoton sc (cameraPos cam)) phs
   in map tf phs'

renderPhotons :: (ObjectCollection o) => Scene o -> Camera -> Integer -> [Photon] -> FrameBuffer
renderPhotons sc cam px phs = rasterPhotons (cameraFrameBlank cam px) $ renderPhotons' sc cam px phs

-- Returns True if there is an unobstructed path from the photon to a point
occludePhoton :: (ObjectCollection o) => Scene o -> Vec3d -> Photon -> Bool
occludePhoton sc cp (Photon p _ _ _) = True -- isNothing $ probeCollection (objects sc) (Ray p (normalize (cp |-| p)))

-- Returns True if the photon is in view of the camera
cullPhoton :: Camera -> Photon -> Bool
cullPhoton _ _ = True

-- Rasters photon in pixel-space to output pixels
-- TODO: this only affects the pixel the photon is it.  It should use the gaussian
rasterPhoton :: FrameBuffer -> Photon -> FrameBuffer
rasterPhoton fb (Photon (V3 x y _) pPow _ _) = fbAccum (|+|) fb [(fmap (fromInteger . floor) (V2 x y), pPow)]

rasterPhotons :: FrameBuffer -> [Photon] -> FrameBuffer
rasterPhotons frame = foldl (rasterPhoton) frame
