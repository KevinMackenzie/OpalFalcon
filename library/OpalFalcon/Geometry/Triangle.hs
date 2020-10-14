module OpalFalcon.Geometry.Triangle
  ( Triangle (..),
    baryToWorld,
    containsPoint,
  )
where

import OpalFalcon.Math.Vector

data Triangle = Tri !Vec3d !Vec3d !Vec3d

{-# INLINE baryToWorld #-}
baryToWorld :: Triangle -> Vec3d -> Vec3d
baryToWorld (Tri a b c) (V3 u v w) =
  (u *| a) |+| (v *| b) |+| (w *| c)

{-# INLINE containsPoint #-}
containsPoint :: Triangle -> Vec3d -> Bool
containsPoint (Tri p0 p1 p2) pt =
  let v0 = p1 |-| p0
      v1 = p2 |-| p1
      v2 = p0 |-| p2
      w0 = pt |-| p0
      w1 = pt |-| p1
      w2 = pt |-| p2
      a = (v0 |><| w0)
      b = (v1 |><| w1)
      c = (v2 |><| w2)
   in (a |.| b) >= 0 && (b |.| c) >= 0 && (c |.| a) >= 0
