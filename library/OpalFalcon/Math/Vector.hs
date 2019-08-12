{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
module OpalFalcon.Math.Vector where

import Data.Word (Word8)
import Data.Functor.Apply (Apply, Functor, (<.>), liftF2)

-- Base typeclasses
class (Foldable a, Apply a) => Vector a where
    constVec :: b -> a b

-- Arithmetic operations on vectors
(|+|) :: (Vector a, Num b) => a b -> a b -> a b
(|-|) :: (Vector a, Num b) => a b -> a b -> a b
(|*|) :: (Vector a, Num b) => a b -> a b -> a b

(|+|) = liftF2 (+)
(|-|) = liftF2 (-)
(|*|) = liftF2 (*)

-- Approximately equal to with default epsilon
(~=) :: (Vector a, Ord b, Floating b) => a b -> a b -> Bool
(~=) = approxEq 0.00001

-- Used to see if two vectors are approximately equal to 
--  handle floating-point errors
approxEq :: (Vector a, Ord b, Floating b) => b -> a b -> a b -> Bool
approxEq e v0 v1 = foldr (&&) True $ liftF2 (\x0 x1 -> (abs $ x0-x1) < e) v0 v1

-- Higher-order types
data Vec2 a = V2 a a
    deriving (Show, Eq, Foldable, Functor)
data Vec3 a = V3 a a a
    deriving (Show, Eq, Foldable, Functor)
data Vec4 a = V4 a a a a
    deriving (Show, Eq, Foldable, Functor)

-- Instances for low-dimmension vectors
instance Apply Vec2 where
    (<.>) (V2 fx fy) (V2 x y)  = (V2 (fx x) (fy y))
instance Vector Vec2 where
    constVec v = (V2 v v)

instance Apply Vec3 where
    (<.>) (V3 fx fy fz) (V3 x y z)  = (V3 (fx x) (fy y) (fz z))
instance Vector Vec3 where
    constVec v = (V3 v v v)

instance Apply Vec4 where
    (<.>) (V4 fx fy fz fw) (V4 x y z w)  = (V4 (fx x) (fy y) (fz z) (fw w))
instance Vector Vec4 where
    constVec v = (V4 v v v v)

-- Vector type specializations
type Vec2d = Vec2 Double
type Vec2i = Vec2 Int

type Vec3d = Vec3 Double
type Vec3i = Vec3 Int

origin :: (Vector a, Num b) => a b
origin = constVec $ fromInteger 0

-- Specialized for homogeneous coords
type Vec4d = Vec4 Double

type ColorRGB = Vec3 Data.Word.Word8
type ColorRGBA = Vec4 Data.Word.Word8
type ColorRGBf = Vec3 Float
type ColorRGBAf = Vec4 Float

white :: ColorRGB
white = constVec 255
whitef :: ColorRGBf
whitef = constVec 1.0

gray :: (Vector a) => b -> a b
gray = constVec

black :: (Num a) => Vec3 a
black = constVec (fromInteger 0)

-- Multiply by scalar
(*|) :: (Vector a, Num b) => b -> a b -> a b
(*|) s = fmap (s*)
(|*) :: (Vector a, Num b) => a b -> b -> a b
(|*) v s = s *| v
-- Dot product
(|.|) :: (Vector a, Num b) => a b -> a b -> b
(|.|) v0 v1 = foldr (+) 0 $ v0 |*| v1
-- Negate
negateVec :: (Vector a, Num b) => a b -> a b
negateVec = fmap negate
-- Distance function
distance :: (Vector a, Floating b) => a b -> a b -> b
distance v0 v1 = mag $ v0 |-| v1

-- Magnitude
mag :: (Vector a, Floating b) => a b -> b
mag v = sqrt $ foldr (+) 0 $ v |*| v
-- Normalize
normalize :: (Vector a, Floating b) => a b -> a b
normalize v = fmap ( / (mag v)) v
-- Clamp
clamp :: (Vector a, Ord b) => a b -> a b -> a b
clamp = liftF2 (\x y -> if x < y then x else y)
-- Cross Product (only defined on vec3's)
(|><|) :: (Num a) => (Vec3 a) -> (Vec3 a) -> (Vec3 a)
(|><|) (V3 a1 a2 a3) (V3 b1 b2 b3) = (V3 (a2*b3-a3*b2) (a3*b1-a1*b3) (a1*b2-a2*b1))

toPixel :: ColorRGBf -> ColorRGB
toPixel = fmap (round . (255*))

-- Functions for converting vectors to and from homogeneous coords
fromHomo :: (Floating a) => Vec4 a -> Vec3 a
fromHomo (V4 x y z w) = V3 (x/w) (y/w) (z/w)
toHomo :: (Num a) => Vec3 a -> a -> Vec4 a
toHomo (V3 x y z) w = (V4 x y z w)
toHomoPos :: (Num a) => Vec3 a -> Vec4 a
toHomoPos v = toHomo v 1
toHomoDir :: (Num a) => Vec3 a -> Vec4 a
toHomoDir v = toHomo v 0

-- Reflection assumes 'norm' is normalized
--  Note: 'v' is centered at the origin; 'incoming' vectors must be flipped
reflect :: (Floating a) => Vec3 a -> Vec3 a -> Vec3 a
reflect v norm = ((2 * (v |.| norm)) *| norm) |-| v

