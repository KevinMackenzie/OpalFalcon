module OpalFalcon.Math.Matrix where

import OpalFalcon.Math.Vector
import qualified Data.Vector.Fixed as V
import qualified Data.Vector.Fixed.Primitive as VPrim
import qualified Data.Vector.Fixed.Boxed as VB

-- Column major matrices
--  We have to define Prim on Vec4s
type Matrix3 a = VB.Vec3 (Vec3 a)
type Matrix4 a = VB.Vec4 (Vec4 a)

mkM3 :: (Vec3 a) -> (Vec3 a) -> (Vec3 a) -> Matrix3 a
mkM3 = V.mk3
mkM4 :: (Vec4 a) -> (Vec4 a) -> (Vec4 a) -> (Vec4 a) -> Matrix4 a
mkM4 = V.mk4

xCol :: Matrix4 a -> Vec4 a
yCol :: Matrix4 a -> Vec4 a
zCol :: Matrix4 a -> Vec4 a
wCol :: Matrix4 a -> Vec4 a

xCol m = m V.! 0
yCol m = m V.! 1
zCol m = m V.! 2
wCol m = m V.! 3

type Matrix3d = Matrix3 Double
type Matrix4d = Matrix4 Double

identity :: (Num a, VPrim.Prim a) => Matrix4 a
identity = (mkM4 (mkV4 1 0 0 0)
               (mkV4 0 1 0 0)
               (mkV4 0 0 1 0)
               (mkV4 0 0 0 1))

transpose :: (VPrim.Prim a) => Matrix4 a -> Matrix4 a
transpose m = 
    let a00 = x4Pos $ xCol m
        a01 = y4Pos $ xCol m
        a02 = z4Pos $ xCol m
        a03 = w4Pos $ xCol m
        a10 = x4Pos $ yCol m
        a11 = y4Pos $ yCol m
        a12 = z4Pos $ yCol m
        a13 = w4Pos $ yCol m
        a20 = x4Pos $ zCol m
        a21 = y4Pos $ zCol m
        a22 = z4Pos $ zCol m
        a23 = w4Pos $ zCol m
        a30 = x4Pos $ wCol m
        a31 = y4Pos $ wCol m
        a32 = z4Pos $ wCol m
        a33 = w4Pos $ wCol m
    in  (mkM4 (mkV4 a00 a10 a20 a30) 
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
    let c0 = xCol m0
        c1 = yCol m0
        c2 = zCol m0
        c3 = wCol m0
        a00 = x4Pos $ xCol m
        a01 = y4Pos $ xCol m
        a02 = z4Pos $ xCol m
        a03 = w4Pos $ xCol m
        a10 = x4Pos $ yCol m
        a11 = y4Pos $ yCol m
        a12 = z4Pos $ yCol m
        a13 = w4Pos $ yCol m
        a20 = x4Pos $ zCol m
        a21 = y4Pos $ zCol m
        a22 = z4Pos $ zCol m
        a23 = w4Pos $ zCol m
        a30 = x4Pos $ wCol m
        a31 = y4Pos $ wCol m
        a32 = z4Pos $ wCol m
        a33 = w4Pos $ wCol m
    in  (mkM4 ((a00 *| c0) |+| (a01 *| c1) |+| (a02 *| c2) |+| (a03 *| c3))
        ((a10 *| c0) |+| (a11 *| c1) |+| (a12 *| c2) |+| (a13 *| c3))
        ((a20 *| c0) |+| (a21 *| c1) |+| (a22 *| c2) |+| (a23 *| c3))
        ((a30 *| c0) |+| (a31 *| c1) |+| (a32 *| c2) |+| (a33 *| c3)))

invert :: Matrix4 a -> Matrix4 a
invert _ = undefined -- TODO

