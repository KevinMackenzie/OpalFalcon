module OpalFalcon.Math.Matrix where

import OpalFalcon.Math.Vector
import qualified Data.Vector.Fixed.Primitive as VPrim

-- Column major matrices
--  We have to define Prim on Vec4s
type Matrix3 a = Vec3 (Vec3 a)
type Matrix4 a = Vec4 (Vec4 a)

type Matrix3d = Matrix3 Double
type Matrix4d = Matrix4 Double

identity :: (Num a, VPrim.Prim a) => Matrix4 a
identity = (mkV4 (mkV4 1 0 0 0)
               (mkV4 0 1 0 0)
               (mkV4 0 0 1 0)
               (mkV4 0 0 0 1))

transpose :: (VPrim.Prim a) => Matrix4 a -> Matrix4 a
transpose m = 
    let a00 = x4Pos $ x4Pos m
        a01 = y4Pos $ x4Pos m
        a02 = z4Pos $ x4Pos m
        a03 = w4Pos $ x4Pos m
        a10 = x4Pos $ y4Pos m
        a11 = y4Pos $ y4Pos m
        a12 = z4Pos $ y4Pos m
        a13 = w4Pos $ y4Pos m
        a20 = x4Pos $ z4Pos m
        a21 = y4Pos $ z4Pos m
        a22 = z4Pos $ z4Pos m
        a23 = w4Pos $ z4Pos m
        a30 = x4Pos $ w4Pos m
        a31 = y4Pos $ w4Pos m
        a32 = z4Pos $ w4Pos m
        a33 = w4Pos $ w4Pos m
    in  (mkV4 (mkV4 a00 a10 a20 a30) 
        (mkV4 a01 a11 a21 a31) 
        (mkV4 a02 a12 a22 a32) 
        (mkV4 a03 a13 a23 a33))

(||*||) :: (Num a, VPrim.Prim a) => Matrix4 a -> Matrix4 a -> Matrix4 a
-- TODO: we should optimize for identity case
-- (||*||) m0 (mkV4 (mkV4 1 0 0 0)
--                (mkV4 0 1 0 0)
--                (mkV4 0 0 1 0)
--                (mkV4 0 0 0 1)) = m0
(||*||) m0 m = 
    let c0 = x4Pos m0
        c1 = y4Pos m0
        c2 = z4Pos m0
        c3 = w4Pos m0
        a00 = x4Pos $ x4Pos m
        a01 = y4Pos $ x4Pos m
        a02 = z4Pos $ x4Pos m
        a03 = w4Pos $ x4Pos m
        a10 = x4Pos $ y4Pos m
        a11 = y4Pos $ y4Pos m
        a12 = z4Pos $ y4Pos m
        a13 = w4Pos $ y4Pos m
        a20 = x4Pos $ z4Pos m
        a21 = y4Pos $ z4Pos m
        a22 = z4Pos $ z4Pos m
        a23 = w4Pos $ z4Pos m
        a30 = x4Pos $ w4Pos m
        a31 = y4Pos $ w4Pos m
        a32 = z4Pos $ w4Pos m
        a33 = w4Pos $ w4Pos m
    in  (mkV4 ((a00 *| c0) |+| (a01 *| c1) |+| (a02 *| c2) |+| (a03 *| c3))
        ((a10 *| c0) |+| (a11 *| c1) |+| (a12 *| c2) |+| (a13 *| c3))
        ((a20 *| c0) |+| (a21 *| c1) |+| (a22 *| c2) |+| (a23 *| c3))
        ((a30 *| c0) |+| (a31 *| c1) |+| (a32 *| c2) |+| (a33 *| c3)))

invert :: Matrix4 a -> Matrix4 a
invert _ = undefined -- TODO

