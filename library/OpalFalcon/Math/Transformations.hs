module OpalFalcon.Math.Transformations where

import OpalFalcon.Math.Matrix
import OpalFalcon.Math.Vector

identity :: (Num a) => Matrix4 a
identity = fmap (\x -> fmap fromInteger x) (V4 (V4 1 0 0 0) (V4 0 1 0 0) (V4 0 0 1 0) (V4 0 0 0 1))

translate :: (Num a) => Vec3 a -> Matrix4 a
translate d = 
    let (V4 c0 c1 c2 _) = identity
    in  (V4 c0 c1 c2 (toHomoPos d))

scale :: (Num a) => Vec3 a -> Matrix4 a
scale (V3 x y z) = (V4 (V4 x 0 0 0) (V4 0 y 0 0) (V4 0 0 z 0) (V4 0 0 0 1))

scaleUniform :: (Num a) => a -> Matrix4 a
scaleUniform s = scale (V3 s s s)

flipY :: (Num a) => Matrix4 a
flipY = (V4 (V4 1 0 0 0)
            (V4 0 (-1) 0 0)
            (V4 0 0 1 0)
            (V4 0 0 0 1))

lookAtFromPos :: (Floating a, Ord a) => Vec3 a -> Vec3 a -> Matrix4 a
lookAtFromPos from to = lookAt $ to |-| from

lookAt :: (Floating a, Ord a) => Vec3 a -> Matrix4 a
lookAt v =
    let forward = normalize v
        tmp = V3 0 1 0
        d = forward |.| tmp
        right = tmp |><| forward
        up = forward |><| right
    in  if right ~= origin then (if d > 0 then identity else flipY)
                           else transpose (V4 (toHomoDir right) (toHomoDir up) (toHomoDir forward) (V4 0 0 0 1))


perspective :: (Floating a) => a -> a -> a -> Matrix4 a
perspective near far fov = 
    (V4 (V4 s 0 0 0) (V4 0 s 0 0) (V4 0 0 ((-far)/d) (-(far*near)/d)) (V4 0 0 (-1) 0))
    where s = 1/(tan ((fov*pi)/(360)))
          d = far-near

applyTransform :: (Num a) => Matrix4 a -> Vec4 a -> Vec4 a
applyTransform m v =
    let t = transpose m
    in  fmap (v |.|) t

applyTransform3 :: (Fractional a) => Matrix4 a -> Vec3 a -> Vec3 a
applyTransform3 m v = fromHomo $ applyTransform m $ toHomoPos v

applyTransformStack :: (Foldable a, Num b) => a (Matrix4 b) -> Vec4 b -> Vec4 b
applyTransformStack s = applyTransform (foldr (||*||) identity s)

