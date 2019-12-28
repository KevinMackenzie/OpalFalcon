{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
module OpalFalcon.Math.Vector where

import Data.Word (Word8)
import Control.Applicative(Applicative, (<*>), liftA2)
import System.Random (Random(..), RandomGen)
import GHC.Float (float2Double, double2Float)

-- Base typeclasses
class (Foldable a, Applicative a) => Vector a where {}

constVec :: Vector a => b -> a b
constVec = pure

vecZip :: Vector a => (b -> c -> d) -> a b -> a c -> a d
vecZip = liftA2

-- Arithmetic operations on vectors
(|+|) :: (Vector a, Num b) => a b -> a b -> a b
(|-|) :: (Vector a, Num b) => a b -> a b -> a b
(|*|) :: (Vector a, Num b) => a b -> a b -> a b

(|+|) = vecZip (+)
(|-|) = vecZip (-)
(|*|) = vecZip (*)

-- Approximately equal to with default epsilon
(~=) :: (Vector a, Ord b, Fractional b) => a b -> a b -> Bool
(~=) = approxEq 0.00001

-- Used to see if two vectors are approximately equal to 
--  handle floating-point errors
approxEq :: (Vector a, Ord b, Fractional b) => b -> a b -> a b -> Bool
approxEq e v0 v1 = foldr (&&) True $ liftA2 (\x0 x1 -> (abs $ x0-x1) < e) v0 v1

-- If we don't support pattern matching (like using vectors/arrays) we don't need 'data' declarations for each vector length
-- Higher-order types
data Vec2 a = V2 !a !a
    deriving (Show, Eq, Foldable, Functor)
data Vec3 a = V3 !a !a !a
    deriving (Show, Eq, Foldable, Functor)
data Vec4 a = V4 !a !a !a !a
    deriving (Show, Eq, Foldable, Functor)

-- Instances for low-dimmension vectors
instance Applicative Vec2 where
    pure x = V2 x x
    (<*>) (V2 fx fy) (V2 x y)  = (V2 (fx x) (fy y))
instance Vector Vec2 where {}

instance Applicative Vec3 where
    pure x = V3 x x x
    (<*>) (V3 fx fy fz) (V3 x y z)  = (V3 (fx x) (fy y) (fz z))
instance Vector Vec3 where {}

instance Applicative Vec4 where
    pure x = V4 x x x x
    (<*>) (V4 fx fy fz fw) (V4 x y z w)  = (V4 (fx x) (fy y) (fz z) (fw w))
instance Vector Vec4 where {}

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

xPos :: Vec3 a -> a
yPos :: Vec3 a -> a
zPos :: Vec3 a -> a

xPos (V3 x _ _) = x
yPos (V3 _ y _) = y
zPos (V3 _ _ z) = z

mutX :: Vec3 a -> a -> Vec3 a
mutY :: Vec3 a -> a -> Vec3 a
mutZ :: Vec3 a -> a -> Vec3 a

mutX (V3 _ y z) x = V3 x y z
mutY (V3 x _ z) y = V3 x y z
mutZ (V3 x y _) z = V3 x y z

-- (r, theta)
-- type PolarCoords = Vec2d
-- toPolar :: Vec2d
-- toPolar (V2 x y) = V2 (sqrt $ x*x + y*y) (atan x y)

-- Represents spherical coordinates (radius, theta, psi) where theta is the polar angle and psi is the azimuth angle
-- type SphereCoords = Vec3d
-- toSphere :: Vec3d -> SphereCoords
-- toSphere (V3 x y z) = V3 (sqrt $ x*x + y*y + z*z) (atan x y) (atan z $ sqrt $ x*x + y*y)

-- Multiply by scalar
(*|) :: (Vector a, Num b) => b -> a b -> a b
(*|) s = fmap (s*)
(|*) :: (Vector a, Num b) => a b -> b -> a b
(|*) v s = fmap (*s) v
-- Divide by scalar
(/|) :: (Vector a, Fractional b) => b -> a b -> a b
(/|) s = fmap (s/)
(|/) :: (Vector a, Fractional b) => a b -> b -> a b
(|/) v s = fmap (/s) v
-- Dot product
(|.|) :: (Vector a, Num b) => a b -> a b -> b
(|.|) v0 v1 = foldr (+) 0 $ v0 |*| v1
-- Negate
negateVec :: (Vector a, Num b) => a b -> a b
negateVec = fmap negate
-- Promote* converts lower order vectors into higher order ones
promote3 :: (Num a) => Vec2 a -> Vec3 a
promote3 (V2 x y) = V3 x y 0
-- Demote* converts higher order vectors into lower order ones
demote4 :: Vec4 a -> Vec3 a
demote4 (V4 x y z _) = V3 x y z
demote3 :: Vec3 a -> Vec2 a
demote3 (V3 x y _) = V2 x y
-- Distance function
distance :: (Vector a, Floating b) => a b -> a b -> b
distance v0 v1 = mag $ v0 |-| v1
-- Magnitude
mag :: (Vector a, Floating b) => a b -> b
mag v = sqrt $ foldr (+) 0 $ v |*| v
-- Sum
vecSum :: (Vector a, Fractional b, Foldable c) => c (a b) -> a b
vecSum = foldr (|+|) origin
-- Average
vecAverage :: (Vector a, Fractional b, Foldable c) => c (a b) -> a b
vecAverage l = 
    let len = length l
    in  case len of
            0 -> origin
            _ -> (vecSum l) |* (1.0 / (fromInteger $ toInteger $ len))
vecAvgComp :: (Vector a, Fractional b) => a b -> b
vecAvgComp v = (foldl (+) 0 v) / (fromInteger $ toInteger $ length v)
-- Normalize
normalize :: (Vector a, Floating b) => a b -> a b
normalize v = fmap ( / (mag v)) v
-- Clamp
clamp :: (Vector a, Ord b) => a b -> a b -> a b
clamp = liftA2 (\x y -> if x < y then x else y)
-- Cross Product (only defined on vec3's)
(|><|) :: (Num a) => (Vec3 a) -> (Vec3 a) -> (Vec3 a)
(|><|) (V3 a1 a2 a3) (V3 b1 b2 b3) = (V3 (a2*b3-a3*b2) (a3*b1-a1*b3) (a1*b2-a2*b1))

-- TODO: dynamic range...?
toPixel :: ColorRGBf -> ColorRGB
toPixel = fmap (round . (255*))

-- Functions for converting vectors to and from homogeneous coords
fromHomo :: (Fractional a) => Vec4 a -> Vec3 a
fromHomo (V4 x y z w) = V3 (x/w) (y/w) (z/w)
fromHomoDir :: Vec4 a -> Vec3 a
fromHomoDir = demote4
toHomo :: (Num a) => Vec3 a -> a -> Vec4 a
toHomo (V3 x y z) w = (V4 x y z w)
toHomoPos :: (Num a) => Vec3 a -> Vec4 a
toHomoPos v = toHomo v 1
toHomoDir :: (Num a) => Vec3 a -> Vec4 a
toHomoDir v = toHomo v 0

-- Reflection assumes 'norm' is normalized
--  Note: 'v' is centered at the origin; 'incoming' vectors must be flipped
reflect :: (Fractional a) => Vec3 a -> Vec3 a -> Vec3 a
reflect v norm = ((2 * (v |.| norm)) *| norm) |-| v


-- Random vector generation on the unit sphere.  These vectors can be transformed
--   to make the results not uniformly sampled (i.e. for importance sampling)
randomVec3 :: (Random a, RandomGen g) => (Vec3 a, Vec3 a) -> g -> (Vec3 a, g)
randomVec3 (V3 xmn ymn zmn, V3 xmx ymx zmx) g =         
    let (x, r0) = randomR (xmn, xmx) g
        (y, r1) = randomR (ymn, ymx) r0
        (z, r2) = randomR (zmn, zmx) r1
    in  (V3 x y z, r2)

randomUnitVec3 :: (Random a, Floating a, Ord a, RandomGen g) => g -> (Vec3 a, g)
randomUnitVec3 g
    | mag v > 1 = randomUnitVec3 g0
    | otherwise = (normalize v, g0)
    where (v, g0) = randomVec3 (constVec (-1), constVec 1) g

instance (Random a, Floating a, Ord a) => Random (Vec3 a) where
    randomR = randomVec3
    random = randomUnitVec3

-- Clamps randomly generated vectors into a hemisphere signified by a normal
clampHemisphere :: (Floating a, Ord a) => Vec3 a -> Vec3 a -> Vec3 a
clampHemisphere norm r
            | norm |.| r > 0 = r
            | otherwise      = negateVec r

-- N-dimmensional gaussian kernel (0-centered)
gaussianOrigin :: (Floating a, Vector b) => b a -> a -> a
gaussianOrigin x sig = (1 / ((sqrt 2*pi)*sig)^(length x)) * (exp (-(mag x)^2) / (2*sig^2))

-- N-dimmensional gaussian kernel (arbitrary center)
gaussian :: (Floating a, Vector b) => b a -> b a -> a -> a
gaussian x p sig = gaussianOrigin (x |-| p) sig

-- Converts VecFs to VecDs
float2DoubleVec :: (Vector a) => a Float -> a Double
float2DoubleVec = fmap float2Double

double2FloatVec :: (Vector a) => a Double -> a Float
double2FloatVec = fmap double2Float
