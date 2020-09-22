{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
module OpalFalcon.Math.Vector where

import Data.Word (Word8)
import Control.Applicative(Applicative, (<*>), liftA2)
import System.Random (Random(..), RandomGen)
import GHC.Float (float2Double, double2Float)

-- Base typeclasses
class (Foldable a, Applicative a) => Vector a where {}

{-# INLINE constVec #-}
constVec :: Vector a => b -> a b
constVec = pure

{-# INLINE vecZip #-}
vecZip :: Vector a => (b -> c -> d) -> a b -> a c -> a d
vecZip f !v0 !v1 = liftA2 f v0 v1

-- Arithmetic operations on vectors
(|+|) :: (Vector a, Num b) => a b -> a b -> a b
(|-|) :: (Vector a, Num b) => a b -> a b -> a b
(|*|) :: (Vector a, Num b) => a b -> a b -> a b
(|/|) :: (Vector a, Fractional b) => a b -> a b -> a b

{-# INLINE (|+|) #-}
(|+|) = vecZip (+)
{-# INLINE (|-|) #-}
(|-|) = vecZip (-)
{-# INLINE (|*|) #-}
(|*|) = vecZip (*)
{-# INLINE (|/|) #-}
(|/|) = vecZip (/)

-- Approximately equal to with default epsilon
{-# INLINE (~=) #-}
(~=) :: (Vector a, Ord b, Fractional b) => a b -> a b -> Bool
(~=) = approxEq 0.00001 -- TODO: should this scale based on the exponent of the two floats?

-- Used to see if two vectors are approximately equal to 
--  handle floating-point errors
{-# INLINE approxEq #-}
approxEq :: (Vector a, Ord b, Fractional b) => b -> a b -> a b -> Bool
approxEq e v0 v1 = foldl (&&) True $ liftA2 (\x0 x1 -> (abs $ x0-x1) < e) v0 v1

{-# INLINE (|==|) #-}
(|==|) :: (Vector a, Eq b) => a b -> a b -> Bool
(|==|) x y = foldl (&&) True $ vecZip (==) x y

-- If we don't support pattern matching (like using vectors/arrays) we don't need 'data' declarations for each vector length
-- Higher-order types
data Vec2 a = V2 !a !a
    deriving (Show, Read, Eq, Foldable, Functor)
data Vec3 a = V3 !a !a !a
    deriving (Show, Read, Eq, Foldable, Functor)
data Vec4 a = V4 !a !a !a !a
    deriving (Show, Read, Eq, Foldable, Functor)

-- Instances for low-dimmension vectors
instance Applicative Vec2 where
    {-# INLINE pure #-}
    pure x = V2 x x
    {-# INLINE (<*>) #-}
    (<*>) (V2 fx fy) (V2 x y)  = (V2 (fx x) (fy y))
instance Vector Vec2 where {}

instance Applicative Vec3 where
    {-# INLINE pure #-}
    pure x = V3 x x x
    {-# INLINE (<*>) #-}
    (<*>) (V3 fx fy fz) (V3 x y z)  = (V3 (fx x) (fy y) (fz z))
instance Vector Vec3 where {}

instance Applicative Vec4 where
    {-# INLINE pure #-}
    pure x = V4 x x x x
    {-# INLINE (<*>) #-}
    (<*>) (V4 fx fy fz fw) (V4 x y z w)  = (V4 (fx x) (fy y) (fz z) (fw w))
instance Vector Vec4 where {}

-- Vector type specializations
type Vec2d = Vec2 Double
type Vec2i = Vec2 Int

type Vec3f = Vec3 Float
type Vec3d = Vec3 Double
type Vec3i = Vec3 Int

{-# INLINE origin #-}
origin :: (Vector a, Num b) => a b
origin = constVec $ fromInteger 0

-- Specialized for homogeneous coords
type Vec4d = Vec4 Double

type ColorRGB = Vec3 Data.Word.Word8
type ColorRGBA = Vec4 Data.Word.Word8
type ColorRGBf = Vec3 Float
type ColorRGBAf = Vec4 Float

{-# INLINE white #-}
white :: ColorRGB
white = constVec 255

{-# INLINE whitef #-}
whitef :: (Fractional a) => Vec3 a
whitef = constVec 1.0

{-# INLINE gray #-}
gray :: (Vector a) => b -> a b
gray = constVec

{-# INLINE black #-}
black :: (Num a) => Vec3 a
black = constVec (fromInteger 0)

{-# INLINE xAxis #-}
{-# INLINE yAxis #-}
{-# INLINE zAxis #-}

xAxis :: (Num a) => Vec3 a
yAxis :: (Num a) => Vec3 a
zAxis :: (Num a) => Vec3 a

xAxis = (V3 1 0 0)
yAxis = (V3 0 1 0)
zAxis = (V3 0 0 1)

{-# INLINE xPos #-}
{-# INLINE yPos #-}
{-# INLINE zPos #-}

xPos :: Vec3 a -> a
yPos :: Vec3 a -> a
zPos :: Vec3 a -> a

xPos (V3 x _ _) = x
yPos (V3 _ y _) = y
zPos (V3 _ _ z) = z

{-# INLINE mutX #-}
{-# INLINE mutY #-}
{-# INLINE mutZ #-}

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
{-# INLINE (*|) #-}
(*|) :: (Vector a, Num b) => b -> a b -> a b
(*|) s = fmap (s*)
{-# INLINE (|*) #-}
(|*) :: (Vector a, Num b) => a b -> b -> a b
(|*) v s = fmap (*s) v
-- Divide by scalar
{-# INLINE (/|) #-}
{-# INLINE (|/) #-}
(/|) :: (Vector a, Fractional b) => b -> a b -> a b
(/|) s = fmap (s/)
(|/) :: (Vector a, Fractional b) => a b -> b -> a b
(|/) v s = fmap (/s) v
-- Dot product
{-# INLINE (|.|) #-}
(|.|) :: (Vector a, Num b) => a b -> a b -> b
(|.|) v0 v1 = foldl (+) 0 $ v0 |*| v1
-- Negate
{-# INLINE negateVec #-}
negateVec :: (Vector a, Num b) => a b -> a b
negateVec = fmap negate
-- Promote* converts lower order vectors into higher order ones
{-# INLINE promote3 #-}
promote3 :: (Num a) => Vec2 a -> Vec3 a
promote3 (V2 x y) = V3 x y 0
-- Demote* converts higher order vectors into lower order ones
{-# INLINE demote4 #-}
demote4 :: Vec4 a -> Vec3 a
demote4 (V4 x y z _) = V3 x y z
{-# INLINE demote3 #-}
demote3 :: Vec3 a -> Vec2 a
demote3 (V3 x y _) = V2 x y
-- Distance function
{-# INLINE distance #-}
distance :: (Vector a, Floating b) => a b -> a b -> b
distance v0 v1 = mag $ v0 |-| v1
{-# INLINE distance2 #-}
distance2 :: (Vector a, Num b) => a b -> a b -> b
distance2 v0 v1 = foldl (+) 0 $ fmap (^2) $ v0 |-| v1
-- Magnitude
{-# INLINE mag #-}
mag :: (Vector a, Floating b) => a b -> b
mag v = sqrt $ foldl (+) 0 $ v |*| v
-- Sum
{-# INLINE vecSum #-}
vecSum :: (Vector a, Fractional b, Foldable c) => c (a b) -> a b
vecSum = foldl (|+|) origin
-- Average
{-# INLINE vecAverage #-}
vecAverage :: (Vector a, Fractional b, Foldable c) => c (a b) -> a b
vecAverage l = 
    let len = length l
    in  case len of
            0 -> origin
            _ -> (vecSum l) |* (1.0 / (fromInteger $ toInteger $ len))
{-# INLINE vecAvgComp #-}
vecAvgComp :: (Vector a, Fractional b) => a b -> b
vecAvgComp v = (vecCompSum v) / (fromInteger $ toInteger $ length v)
{-# INLINE vecCompSum #-}
vecCompSum :: (Vector a, Num b) => a b -> b
vecCompSum = foldl (+) 0
-- Normalize
{-# INLINE normalize #-}
normalize :: (Vector a, Eq b, Floating b) => a b -> a b
normalize v = if v |==| origin then origin else fmap ( / (mag v)) v
-- Clamp
{-# INLINE clamp #-}
clamp :: (Vector a, Ord b) => a b -> a b -> a b
clamp = liftA2 (\x y -> if x < y then x else y)
-- Cross Product (only defined on vec3's)
{-# INLINE (|><|) #-}
(|><|) :: (Num a) => (Vec3 a) -> (Vec3 a) -> (Vec3 a)
(|><|) (V3 a1 a2 a3) (V3 b1 b2 b3) = (V3 (a2*b3-a3*b2) (a3*b1-a1*b3) (a1*b2-a2*b1))

-- Functions for converting vectors to and from homogeneous coords
{-# INLINE fromHomo #-}
fromHomo :: (Fractional a) => Vec4 a -> Vec3 a
fromHomo (V4 x y z w) = V3 (x/w) (y/w) (z/w)
{-# INLINE fromHomoDir #-}
fromHomoDir :: Vec4 a -> Vec3 a
fromHomoDir = demote4
{-# INLINE toHomo #-}
toHomo :: (Num a) => Vec3 a -> a -> Vec4 a
toHomo (V3 x y z) w = (V4 x y z w)
{-# INLINE toHomoPos #-}
toHomoPos :: (Num a) => Vec3 a -> Vec4 a
toHomoPos v = toHomo v 1
{-# INLINE toHomoDir #-}
toHomoDir :: (Num a) => Vec3 a -> Vec4 a
toHomoDir v = toHomo v 0

-- Reflection assumes 'norm' is normalized
--  Note: 'v' is centered at the origin; 'incoming' vectors must be flipped
{-# INLINE reflect #-}
reflect :: (Fractional a) => Vec3 a -> Vec3 a -> Vec3 a
reflect v norm = ((2 * (v |.| norm)) *| norm) |-| v


-- Random vector generation on the unit sphere.  These vectors can be transformed
--   to make the results not uniformly sampled (i.e. for importance sampling)
{-# INLINE randomVec3 #-}
randomVec3 :: (Random a, RandomGen g) => (Vec3 a, Vec3 a) -> g -> (Vec3 a, g)
randomVec3 (V3 xmn ymn zmn, V3 xmx ymx zmx) g =         
    let (x, r0) = randomR (xmn, xmx) g
        (y, r1) = randomR (ymn, ymx) r0
        (z, r2) = randomR (zmn, zmx) r1
    in  (V3 x y z, r2)

{-# INLINE randomUnitVec3 #-}
randomUnitVec3 :: (Random a, Floating a, Ord a, RandomGen g) => g -> (Vec3 a, g)
randomUnitVec3 g
    | mag v > 1 = randomUnitVec3 g0
    | otherwise = (normalize v, g0)
    where (v, g0) = randomVec3 (constVec (-1), constVec 1) g

instance (Random a, Floating a, Ord a) => Random (Vec3 a) where
    {-# INLINE randomR #-}
    randomR = randomVec3
    {-# INLINE random #-}
    random = randomUnitVec3

-- Clamps randomly generated vectors into a hemisphere signified by a normal
{-# INLINE clampHemisphere#-}
clampHemisphere :: (Floating a, Ord a) => Vec3 a -> Vec3 a -> Vec3 a
clampHemisphere norm r
            | norm |.| r > 0 = r
            | otherwise      = negateVec r

-- N-dimmensional gaussian kernel (0-centered)
{-# INLINE gaussianOrigin #-}
gaussianOrigin :: (Floating a, Vector b) => b a -> a -> a
gaussianOrigin x sig = (1 / ((sqrt 2*pi)*sig)^(length x)) * (exp (-(mag x)^2) / (2*sig^2))

-- N-dimmensional gaussian kernel (arbitrary center)
{-# INLINE gaussian #-}
gaussian :: (Floating a, Vector b) => b a -> b a -> a -> a
gaussian x p sig = gaussianOrigin (x |-| p) sig

-- Converts VecFs to VecDs
{-# INLINE float2DoubleVec #-}
float2DoubleVec :: (Vector a) => a Float -> a Double
float2DoubleVec = fmap float2Double

{-# INLINE double2FloatVec #-}
double2FloatVec :: (Vector a) => a Double -> a Float
double2FloatVec = fmap double2Float

-- Uses convention of physics (theta = inclination, psi = azimuth)
{-# INLINE fromSphere #-}
fromSphere :: (Floating b) => Vec3 b -> Vec3 b
fromSphere (V3 r theta psi) = (V3 x y z)
    where x = r*sinth*(cos psi)
          y = r*sinth*(sin psi)
          z = r*(cos theta)
          sinth = sin theta

-- Gets a vector orthagonal to the provided vector
{-# INLINE getOrthoVec #-}
getOrthoVec :: (Ord b, Floating b) => Vec3 b -> Vec3 b
getOrthoVec v = normalize $ v |><| (if (mag (v |><| xAxis)) < 0.001 then yAxis else xAxis)

-- Returns true if 'x' is inside the slice of space at pos 'beg' with the vector 'end-beg'
{-# INLINE isBetween #-}
isBetween :: Vec3d -> Vec3d -> Vec3d -> Bool
isBetween beg end x = (end |-| x) |.| (end |-| beg) < 0