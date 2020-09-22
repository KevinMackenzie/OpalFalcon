module OpalFalcon.Math.Matrix where

import OpalFalcon.Math.Vector

-- Column major matrices
type Matrix3 a = Vec3 (Vec3 a)
type Matrix4 a = Vec4 (Vec4 a)

type Matrix3d = Matrix3 Double
type Matrix4d = Matrix4 Double

{-# INLINE identity #-}
identity :: (Num a) => Matrix4 a
identity = (V4 (V4 1 0 0 0)
               (V4 0 1 0 0)
               (V4 0 0 1 0)
               (V4 0 0 0 1))

-- TODO: Make transpose more general:
-- transpose :: (Vector v0, Vector v1) => v0 (v1 a) -> v1 (v0 a)
{-# INLINE transpose #-}
transpose :: Matrix4 a -> Matrix4 a
transpose (V4 (V4 a00 a01 a02 a03) 
              (V4 a10 a11 a12 a13)
              (V4 a20 a21 a22 a23) 
              (V4 a30 a31 a32 a33)) = 
    (V4 (V4 a00 a10 a20 a30) 
        (V4 a01 a11 a21 a31) 
        (V4 a02 a12 a22 a32) 
        (V4 a03 a13 a23 a33))

{-# INLINE (||*||) #-}
(||*||) :: (Vector v0, Vector v1, Vector v2, Num a) => v0 (v1 a) -> v2 (v0 a) -> v2 (v1 a)
-- TODO: can we optimize for identity case?
-- (||*||) m0 (V4 (V4 1 0 0 0)
--                (V4 0 1 0 0)
--                (V4 0 0 1 0)
--                (V4 0 0 0 1)) = m0
(||*||) m0 = fmap (m0 ||*|)

{-# INLINE (||*|) #-}
(||*|) :: (Vector v0, Vector v1, Num a) => v0 (v1 a) -> v0 a -> v1 a
(||*|) m0 v = foldl (|+|) origin $ vecZip (*|) v m0

invert :: Matrix4 a -> Matrix4 a
invert _ = undefined -- TODO

