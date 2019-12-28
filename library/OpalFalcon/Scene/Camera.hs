module OpalFalcon.Scene.Camera where

import Data.Array
import OpalFalcon.Math.Matrix
import OpalFalcon.Math.Ray
import OpalFalcon.Math.Transformations
import OpalFalcon.Math.Vector

-- A simple camera
data Camera
  = Camera
      { cameraPos :: Vec3d,
        cameraDir :: Vec3d,
        cameraFOV :: Double, -- Horizontal FOV in radians
        cameraAspect :: Double -- Aspect ratio of frame (w/h)
      }

-- Row-major pixel bitmap
data FrameBuffer
  = FB
      { fbWidth :: Int,
        fbHeight :: Int,
        fbData :: Array Int ColorRGBf
      }

fbUpdate :: FrameBuffer -> [(Vec2i, ColorRGBf)] -> FrameBuffer
fbUpdate fb a = fbAccum (\_ v -> v) fb a

fbAccum :: (ColorRGBf -> a -> ColorRGBf) -> FrameBuffer -> [(Vec2i, a)] -> FrameBuffer
fbAccum f fb a = FB
  { fbWidth = fbWidth fb,
    fbHeight = fbHeight fb,
    fbData = accum f (fbData fb) $ map (\((V2 x y), v) -> (y * (fbWidth fb) + x, v)) a
  }

cameraRay :: Camera -> Ray
cameraRay c = Ray (cameraPos c) (cameraDir c)

-- Generates the view-matrix for the camera
cameraViewTransform :: Camera -> Matrix4d
cameraViewTransform cam = (translate (negateVec cp)) ||*|| (lookAtFromPos cp cd)
  where
    cp = cameraPos cam
    cd = cameraDir cam

-- Generates the projection-matrix for the camera
cameraProjTransform :: Camera -> Matrix4d
cameraProjTransform cam = perspective 0.1 1000.0 (cameraFOV cam)

-- Generates the transform from normalized screen space to pixel-space
cameraPixelTransform :: Camera -> Integer -> Matrix4d
cameraPixelTransform _ height = scaleUniform (fromInteger height)

-- Generates a blank frame buffer
cameraFrameBlank :: Camera -> Integer -> FrameBuffer
cameraFrameBlank c height =
  FB
    { fbHeight = fromInteger height,
      fbWidth = fromInteger w,
      fbData = listArray (0, l) [black :: ColorRGBf | _ <- [0, l]]
    }
  where
    l = fromInteger $ height * w
    w = floor $ (fromInteger height) * (cameraAspect c)
