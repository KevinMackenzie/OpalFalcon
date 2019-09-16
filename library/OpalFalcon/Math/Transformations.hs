{-# LANGUAGE ScopedTypeVariables #-}
module OpalFalcon.Math.Transformations where

import OpalFalcon.Math.Matrix
import OpalFalcon.Math.Vector
import qualified Data.Vector.Fixed.Primitive as VPrim

-- A Vector space is a possibly invertable transformation from the external space to the internal space
-- data VectorSpace = MkVectorSpace Matrix4d (Maybe Matrix4d)
type VectorSpace = Matrix4d

translate :: (VPrim.Prim a, Num a) => Vec3 a -> Matrix4 a
translate d = mkM4 (mkV4 1 0 0 0)
                   (mkV4 0 1 0 0)
                   (mkV4 0 0 1 0)
                   (mkV4 (xPos d) (yPos d) (zPos d) 1)

scale :: (VPrim.Prim a, Num a) => Vec3 a -> Matrix4 a
scale v = let (x, y, z) = v3T v in (mkM4 (mkV4 x 0 0 0) (mkV4 0 y 0 0) (mkV4 0 0 z 0) (mkV4 0 0 0 1))

scaleUniform :: (VPrim.Prim a, Num a) => a -> Matrix4 a
scaleUniform s = scale (mkV3 s s s)

orthoX :: (VPrim.Prim a, Num a) => Matrix4 a
orthoX = (mkM4 (mkV4 0 0 0 0)
               (mkV4 0 1 0 0)
               (mkV4 0 0 1 0)
               (mkV4 0 0 0 1))
orthoY :: (VPrim.Prim a, Num a) => Matrix4 a
orthoY = (mkM4 (mkV4 1 0 0 0)
             (mkV4 0 0 0 0)
             (mkV4 0 0 0 0)
             (mkV4 0 0 0 1))
orthoZ :: (VPrim.Prim a, Num a) => Matrix4 a
orthoZ = (mkM4 (mkV4 1 0 0 0)
             (mkV4 0 1 0 0)
             (mkV4 0 0 0 0)
             (mkV4 0 0 0 1))

flipY :: (VPrim.Prim a, Num a) => Matrix4 a
flipY = (mkM4 (mkV4 1 0 0 0)
            (mkV4 0 (-1) 0 0)
            (mkV4 0 0 1 0)
            (mkV4 0 0 0 1))

lookAtFromPos :: (VPrim.Prim a, Floating a, Ord a) => Vec3 a -> Vec3 a -> Matrix4 a
lookAtFromPos from to = lookAt $ to |-| from

lookAt :: (VPrim.Prim a, Floating a, Ord a) => Vec3 a -> Matrix4 a
lookAt v =
    let forward = normalize v
        tmp = mkV3 0 1 0
        d = forward |.| tmp
        right = tmp |><| forward
        up = forward |><| right
    in  if right ~= origin then (if d > 0 then identity else flipY)
                           else transpose (mkM4 (toHomoDir right) (toHomoDir up) (toHomoDir forward) (mkV4 0 0 0 1))


perspective :: (VPrim.Prim a, Floating a) => a -> a -> a -> Matrix4 a
perspective near far fov = 
    (mkM4 (mkV4 s 0 0 0) (mkV4 0 s 0 0) (mkV4 0 0 ((-far)/d) (-(far*near)/d)) (mkV4 0 0 (-1) 0))
    where s = 1/(tan ((fov*pi)/(360)))
          d = far-near

applyTransform :: (VPrim.Prim a, Num a) => Matrix4 a -> Vec4 a -> Vec4 a
applyTransform m v =
    let t = transpose m
        xc = xCol t
        yc = yCol t
        zc = zCol t
        wc = wCol t
    in  mkV4 (v |.| xc) (v |.| yc) (v |.| zc) (v |.| wc) -- fmap (v |.|) t

applyTransform3 :: (VPrim.Prim a, Fractional a) => Matrix4 a -> Vec3 a -> Vec3 a
applyTransform3 m v = fromHomo $ applyTransform m $ toHomoPos v

applyTransformStack :: (VPrim.Prim b, Foldable a, Num b) => a (Matrix4 b) -> Vec4 b -> Vec4 b
applyTransformStack s = applyTransform (foldr (||*||) identity s)

zDir :: (VPrim.Prim a) =>  Matrix4 a -> Vec3 a
zDir m = demote4 $ zCol m
-- in general: zDir m = applyTransform m zAxis

spacePos :: (VPrim.Prim a) => Matrix4 a -> Vec3 a
spacePos m = demote4 $ wCol m
-- in general: spacePos m = applyTransform m origin

xAxis :: (VPrim.Prim a, Num a) => Vec3 a
yAxis :: (VPrim.Prim a, Num a) => Vec3 a
zAxis :: (VPrim.Prim a, Num a) => Vec3 a

xAxis = (mkV3 1 0 0)
yAxis = (mkV3 0 1 0)
zAxis = (mkV3 0 0 1)

-- Creates a vector space out of the provided position and basis vectors
mkAffineSpace :: Vec3d -> Vec3d -> Vec3d -> Vec3d -> VectorSpace
mkAffineSpace pos x y z = (mkM4 (toHomoDir x)
                        (toHomoDir y)
                        (toHomoDir z)
                        (toHomoPos pos))

-- TODO: add ortho(normal), non-affine, and invertable space constructors

