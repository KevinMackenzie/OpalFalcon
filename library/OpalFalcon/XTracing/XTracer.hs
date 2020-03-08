module OpalFalcon.XTracing.XTracer where

import OpalFalcon.Math.Vector
import OpalFalcon.Scene
import OpalFalcon.Scene.Camera

class XTracer a where
  renderScene :: (ObjectCollection o) => a -> Scene o -> Int -> Camera -> [ColorRGBf]
