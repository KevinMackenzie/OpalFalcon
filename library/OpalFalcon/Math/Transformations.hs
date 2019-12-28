module OpalFalcon.Math.Transformations where

import OpalFalcon.Math.Matrix
import OpalFalcon.Math.Vector

-- A Vector space is a possibly invertable transformation from the external space to the internal space
-- data VectorSpace = MkVectorSpace Matrix4d (Maybe Matrix4d)
type VectorSpace = Matrix4d

translate :: (Num a) => Vec3 a -> Matrix4 a
translate d = 
    let (V4 c0 c1 c2 _) = identity
    in  (V4 c0 c1 c2 (toHomoPos d))

scale :: (Num a) => Vec3 a -> Matrix4 a
scale (V3 x y z) = (V4 (V4 x 0 0 0) (V4 0 y 0 0) (V4 0 0 z 0) (V4 0 0 0 1))

scaleUniform :: (Num a) => a -> Matrix4 a
scaleUniform s = scale (V3 s s s)

orthoX :: (Num a) => Matrix4 a
orthoX = (V4 (V4 0 0 0 0)
             (V4 0 1 0 0)
             (V4 0 0 1 0)
             (V4 0 0 0 1))
orthoY :: (Num a) => Matrix4 a
orthoY = (V4 (V4 1 0 0 0)
             (V4 0 0 0 0)
             (V4 0 0 1 0)
             (V4 0 0 0 1))
orthoZ :: (Num a) => Matrix4 a
orthoZ = (V4 (V4 1 0 0 0)
             (V4 0 1 0 0)
             (V4 0 0 0 0)
             (V4 0 0 0 1))

flipY :: (Num a) => Matrix4 a
flipY = (V4 (V4 1 0 0 0)
            (V4 0 (-1) 0 0)
            (V4 0 0 1 0)
            (V4 0 0 0 1))

lookAtFromPos :: (Floating a, Ord a) => Vec3 a -> Vec3 a -> Matrix4 a
lookAtFromPos from to = lookAt $ to |-| from

-- Positive z axis is the "forward" direction of the result
lookAt :: (Floating a, Ord a) => Vec3 a -> Matrix4 a
lookAt v =
    let forward = normalize v
        tmp = V3 0 1 0
        d = forward |.| tmp
        right = tmp |><| forward
        up = forward |><| right
    in  if right ~= origin then (if d > 0 then identity else flipY)
                           else transpose (V4 (toHomoDir right) (toHomoDir up) (toHomoDir forward) (V4 0 0 0 1))

-- Allow specification of "up" vector
lookAtCam :: (Floating a, Ord a) => Vec3 a -> Vec3 a -> Matrix4 a
lookAtCam f up =
    let forward = normalize f
        d = forward |.| up
        right = up |><| forward
        up' = forward |><| right
    in transpose (V4 (toHomoDir right) (toHomoDir up') (toHomoDir forward) (V4 0 0 0 1))

-- FOV is the horizontal field-of-view in radians
perspective :: (Floating a) => a -> a -> a -> Matrix4 a
perspective near far fov = 
    (V4 (V4 s 0 0 0) (V4 0 s 0 0) (V4 0 0 ((-far)/d) (-(far*near)/d)) (V4 0 0 (-1) 0))
    where s = 1/(tan (fov / 2))
          d = far-near

applyTransform :: (Num a) => Matrix4 a -> Vec4 a -> Vec4 a
applyTransform m v =
    let t = transpose m
    in  fmap (v |.|) t

applyTransform3 :: (Fractional a) => Matrix4 a -> Vec3 a -> Vec3 a
applyTransform3 m v = fromHomo $ applyTransform m $ toHomoPos v

-- transform stack is in reverse-transform order
-- [a, b, c] v => a * (b * (c * v)) => (a * (b * c)) * v
applyTransformStack :: (Foldable a, Num b) => a (Matrix4 b) -> Vec4 b -> Vec4 b
applyTransformStack s = applyTransform (foldr (||*||) identity s)

applyTransformStack3 :: (Foldable a, Fractional b) => a (Matrix4 b) -> Vec3 b -> Vec3 b
applyTransformStack3 s p = fromHomo $ applyTransform (foldr (||*||) identity s) $ toHomoPos p

zDir :: Matrix4 a -> Vec3 a
zDir (V4 _ _ z _) = demote4 z
-- in general: zDir m = applyTransform m zAxis

spacePos :: Matrix4 a -> Vec3 a
spacePos (V4 _ _ _ w) = demote4 w
-- in general: spacePos m = applyTransform m origin

xAxis :: (Num a) => Vec3 a
yAxis :: (Num a) => Vec3 a
zAxis :: (Num a) => Vec3 a

xAxis = (V3 1 0 0)
yAxis = (V3 0 1 0)
zAxis = (V3 0 0 1)

-- Creates a vector space out of the provided position and basis vectors
mkAffineSpace :: Vec3d -> Vec3d -> Vec3d -> Vec3d -> VectorSpace
mkAffineSpace pos x y z = (V4 (toHomoDir x)
                        (toHomoDir y)
                        (toHomoDir z)
                        (toHomoPos pos))

affineTranslate :: VectorSpace -> Vec3d
affineTranslate (V4 _ _ _ p) = fromHomo p

-- TODO: add ortho(normal), non-affine, and invertable space constructors

