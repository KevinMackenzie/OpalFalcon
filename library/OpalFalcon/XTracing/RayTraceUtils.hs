module OpalFalcon.XTracing.RayTraceUtils where

import Data.Bits
import OpalFalcon.BaseTypes
import OpalFalcon.Math.Optics
import OpalFalcon.Math.Transformations
import OpalFalcon.Math.Vector
import OpalFalcon.Scene.Camera

--                     pos   lightOutDir norm     BSSRDF
type SurfaceRadiance = Vec3d -> Vec3d -> Vec3d -> Bssrdf -> ColorRGBf
--                     pos   lightOutDir phase
type VolumeRadiance = Vec3d -> Vec3d -> PhaseFunc -> ColorRGBf
--                w    h
type ImageSize = (Int, Int)

genPixMap :: (Integral a) => a -> a -> [(a, a)]
genPixMap x2 y2 =
  [ (x, y) | x <- [- x2 .. (x2 -1)], y <- [- y2 .. (y2 -1)]
  ]

genRay :: (Integral a) => Vec3d -> Vec3d -> Vec3d -> (a, a) -> Vec3d
genRay forward up right (x, y) =
  normalize $
    forward |+| ((fromIntegral x) *| right) |+| ((fromIntegral y) *| up)

genRays :: Camera -> Int -> [Ray]
genRays cam h =
  let w = floor $ (cameraAspect cam) * (fromIntegral h)
      w2 = shiftR w 1
      h2 = shiftR h 1
      d = (fromIntegral w2) / (tan $ (cameraFOV cam) / 2)
      forward = d *| (cameraDir cam)
      right = normalize $ forward |><| (cameraUp cam)
      up = normalize $ right |><| (cameraDir cam)
   in map ((Ray $ cameraPos cam) . (genRay forward up right)) $ genPixMap w2 h2
