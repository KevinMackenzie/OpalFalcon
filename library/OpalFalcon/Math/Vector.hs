{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
module OpalFalcon.Math.Vector where

-- TODO: pattern matching is just a convenience that made writing the initial code easier.  We should instead use fixed-length, unboxed vectors for all built-in aliases (for primative data types) and allow the user to modify the contents to generate new vectors and to extract components of the vectors

import Data.Word (Word8)
import Data.Bits ((.&.))
import qualified Data.Vector.Fixed as V
import qualified Data.Vector.Fixed.Primitive as VPrim

type Vec2 = VPrim.Vec2
type Vec3 = VPrim.Vec3
type Vec4 = VPrim.Vec4

-- TODO: we don't need to define these for all types, and we don't want to carry the VPrim.Prim dependency everywhere either...but I don't want to lose generality
mkV2 :: (VPrim.Prim a) => a -> a -> Vec2 a
mkV2 = V.mk2
mkV3 :: (VPrim.Prim a) => a -> a -> a -> Vec3 a
mkV3 = V.mk3
mkV4 :: (VPrim.Prim a) => a -> a -> a -> a -> Vec4 a
mkV4 = V.mk4

type Vec2d = Vec2 Double
type Vec2i = Vec2 Int

type Vec3d = Vec3 Double
type Vec3i = Vec3 Int

-- Specialized for homogeneous coords
type Vec4d = Vec4 Double

type ColorRGB = Vec3 Data.Word.Word8
type ColorRGBA = Vec4 Data.Word.Word8
type ColorRGBf = Vec3 Float
type ColorRGBAf = Vec4 Float

constVec :: (V.Vector v a, VPrim.Prim a) => a -> v a
constVec x = V.generate (\_ -> x)

-- Arithmetic operations on vectors
(|+|) :: (V.Vector a b, Num b) => a b -> a b -> a b
(|-|) :: (V.Vector a b, Num b) => a b -> a b -> a b
(|*|) :: (V.Vector a b, Num b) => a b -> a b -> a b

(|+|) = V.zipWith (+)
(|-|) = V.zipWith (-)
(|*|) = V.zipWith (*)

-- Approximately equal to with default epsilon
(~=) :: (VPrim.Prim b, Ord b, Fractional b) => Vec3 b -> Vec3 b -> Bool
(~=) = approxEq 0.00001

-- TODO: this should handle vectors of any length
-- Used to see if two vectors are approximately equal to 
--  handle floating-point errors
approxEq :: (VPrim.Prim b, Ord b, Num b) => b -> Vec3 b -> Vec3 b -> Bool
approxEq e v0 v1 = (V.foldr (.&.) (1 :: Int) $ V.zipWith (\x0 x1 -> if (abs $ x0-x1) < e then 1 else 0) v0 v1) == 1 

origin :: (V.Vector a b, VPrim.Prim b, Num b) => a b
origin = constVec 0

white :: ColorRGB
white = constVec 255
whitef :: ColorRGBf
whitef = constVec 1.0

gray :: (V.Vector a b, VPrim.Prim b) => b -> a b
gray = constVec

black :: (V.Vector a b, VPrim.Prim b, Num b) => a b
black = constVec 0

xPos :: (VPrim.Prim a) => Vec3 a -> a
yPos :: (VPrim.Prim a) => Vec3 a -> a
zPos :: (VPrim.Prim a) => Vec3 a -> a

xPos v = v V.! 0
yPos v = v V.! 1
zPos v = v V.! 2

mutX :: (VPrim.Prim a) => Vec3 a -> a -> Vec3 a
mutY :: (VPrim.Prim a) => Vec3 a -> a -> Vec3 a
mutZ :: (VPrim.Prim a) => Vec3 a -> a -> Vec3 a

mutX v x = mkV3 x         (v V.! 1) (v V.! 2)
mutY v y = mkV3 (v V.! 0) y         (v V.! 2)
mutZ v z = mkV3 (v V.! 0) (v V.! 1) z

x4Pos :: (VPrim.Prim a) => Vec4 a -> a
y4Pos :: (VPrim.Prim a) => Vec4 a -> a
z4Pos :: (VPrim.Prim a) => Vec4 a -> a
w4Pos :: (VPrim.Prim a) => Vec4 a -> a

x4Pos v = v V.! 0
y4Pos v = v V.! 1
z4Pos v = v V.! 2
w4Pos v = v V.! 3

-- (r, theta)
-- type PolarCoords = Vec2d
-- toPolar :: Vec2d
-- toPolar (V2 x y) = V2 (sqrt $ x*x + y*y) (atan x y)

-- Represents spherical coordinates (radius, theta, psi) where theta is the polar angle and psi is the azimuth angle
-- type SphereCoords = Vec3d
-- toSphere :: Vec3d -> SphereCoords
-- toSphere (V3 x y z) = V3 (sqrt $ x*x + y*y + z*z) (atan x y) (atan z $ sqrt $ x*x + y*y)

-- Multiply by scalar
(*|) :: (V.Vector a b, Num b) => b -> a b -> a b
(*|) s = V.map (s*)
(|*) :: (V.Vector a b, Num b) => a b -> b -> a b
(|*) v s = s *| v
-- Dot product
(|.|) :: (V.Vector a b, Num b) => a b -> a b -> b
(|.|) v0 v1 = V.foldr (+) 0 $ v0 |*| v1
-- Negate
negateVec :: (V.Vector a b, Num b) => a b -> a b
negateVec = V.map negate
-- Promote* converts lower order vectors into higher order ones
promote3 :: (VPrim.Prim a, Num a) => Vec2 a -> Vec3 a
promote3 v = mkV3 (v V.! 0) (v V.! 1) 0
-- Demote* converts higher order vectors into lower order ones
demote4 :: VPrim.Prim a => Vec4 a -> Vec3 a
demote4 v = mkV3 (v V.! 0) (v V.! 1) (v V.! 2)
demote3 :: VPrim.Prim a => Vec3 a -> Vec2 a
demote3 v = mkV2 (v V.! 0) (v V.! 1)
-- Distance function
distance :: (V.Vector a b, Floating b) => a b -> a b -> b
distance v0 v1 = mag $ v0 |-| v1
-- Magnitude
mag :: (V.Vector a b, Floating b) => a b -> b
mag v = sqrt $ V.foldr (+) 0 $ v |*| v
-- Sum
vecSum :: (V.Vector a b, VPrim.Prim b, Fractional b, Foldable c) => c (a b) -> a b
vecSum = foldr (|+|) origin
-- Average
vecAverage :: (V.Vector a b, VPrim.Prim b, Fractional b, Foldable c) => c (a b) -> a b
vecAverage l = 
    let len = length l
    in  case len of
            0 -> origin
            _ -> (vecSum l) |* (1.0 / (fromInteger $ toInteger $ len))
-- Normalize
normalize :: (V.Vector a b, VPrim.Prim b, Floating b) => a b -> a b
normalize v = V.map ( / (mag v)) v
-- Clamp
clamp :: (V.Vector a b, Ord b) => a b -> a b -> a b
clamp = V.zipWith (\x y -> if x < y then x else y)
-- Cross Product (only defined on vec3's)
(|><|) :: (VPrim.Prim a, Num a) => (Vec3 a) -> (Vec3 a) -> (Vec3 a)
(|><|) a b = 
    let a1 = xPos a
        a2 = yPos a
        a3 = zPos a
        b1 = xPos b
        b2 = yPos b
        b3 = zPos b
    in  mkV3 (a2*b3-a3*b2) (a3*b1-a1*b3) (a1*b2-a2*b1)

toPixel :: ColorRGBf -> ColorRGB
toPixel = V.map (round . (255*))

-- Functions for converting vectors to and from homogeneous coords
fromHomo :: (VPrim.Prim a, Fractional a) => Vec4 a -> Vec3 a
fromHomo v = 
    let x = v V.! 0
        y = v V.! 1
        z = v V.! 2
        w = v V.! 3
    in  mkV3 (x/w) (y/w) (z/w)
toHomo :: (VPrim.Prim a, Num a) => Vec3 a -> a -> Vec4 a
toHomo v w = mkV4 (xPos v) (yPos v) (zPos v) w
toHomoPos :: (VPrim.Prim a, Num a) => Vec3 a -> Vec4 a
toHomoPos v = toHomo v 1
toHomoDir :: (VPrim.Prim a, Num a) => Vec3 a -> Vec4 a
toHomoDir v = toHomo v 0

-- Reflection assumes 'norm' is normalized
--  Note: 'v' is centered at the origin; 'incoming' vectors must be flipped
reflect :: (VPrim.Prim a, Fractional a) => Vec3 a -> Vec3 a -> Vec3 a
reflect v norm = ((2 * (v |.| norm)) *| norm) |-| v

