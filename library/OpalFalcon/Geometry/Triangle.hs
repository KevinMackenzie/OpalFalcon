module OpalFalcon.Geometry.Triangle
  ( Triangle (..),
    baryToWorld,
  )
where

import OpalFalcon.Math.Vector

data Triangle = Tri !Vec3d !Vec3d !Vec3d

{-# INLINE baryToWorld #-}
baryToWorld :: Triangle -> Vec3d -> Vec3d
baryToWorld (Tri a b c) (V3 u v w) =
  (u *| a) |+| (v *| b) |+| (w *| c)
