module OpalFalcon.Math.Matrix where

import OpalFalcon.Math.Vector

-- Column major matrices
type Matrix3 a = Vec3 (Vec3 a)
type Matrix4 a = Vec4 (Vec4 a)

type Matrix3d = Matrix3 Double
type Matrix4d = Matrix4 Double

identity :: (Num a) => Matrix4 a
identity = (V4 (V4 1 0 0 0)
               (V4 0 1 0 0)
               (V4 0 0 1 0)
               (V4 0 0 0 1))

transpose :: Matrix4 a -> Matrix4 a
transpose (V4 (V4 a00 a01 a02 a03) 
              (V4 a10 a11 a12 a13)
              (V4 a20 a21 a22 a23) 
              (V4 a30 a31 a32 a33)) = 
    (V4 (V4 a00 a10 a20 a30) 
        (V4 a01 a11 a21 a31) 
        (V4 a02 a12 a22 a32) 
        (V4 a03 a13 a23 a33))

(||*||) :: (Num a) => Matrix4 a -> Matrix4 a -> Matrix4 a
-- TODO: we should optimize for identity case
-- (||*||) m0 (V4 (V4 1 0 0 0)
--                (V4 0 1 0 0)
--                (V4 0 0 1 0)
--                (V4 0 0 0 1)) = m0
(||*||) (V4 c0 c1 c2 c3) 
        (V4 (V4 a00 a01 a02 a03) 
            (V4 a10 a11 a12 a13) 
            (V4 a20 a21 a22 a23) 
            (V4 a30 a31 a32 a33)) = 
    (V4 ((a00 *| c0) |+| (a01 *| c1) |+| (a02 *| c2) |+| (a03 *| c3))
        ((a10 *| c0) |+| (a11 *| c1) |+| (a12 *| c2) |+| (a13 *| c3))
        ((a20 *| c0) |+| (a21 *| c1) |+| (a22 *| c2) |+| (a23 *| c3))
        ((a30 *| c0) |+| (a31 *| c1) |+| (a32 *| c2) |+| (a33 *| c3)))

invert :: Matrix4 a -> Matrix4 a
invert _ = undefined -- TODO

