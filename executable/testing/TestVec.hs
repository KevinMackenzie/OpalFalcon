module Main where

-- data Vec2 a = Vec2 {-# UNPACK #-} !a !a deriving Show
data Vec3 a = Vec3 {-# UNPACK #-} !a !a !a deriving Show
-- data Vec4 a = Vec4 {-# UNPACK #-} !a !a !a !a deriving Show

(|+|) (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1 + x2) (y1 + y2) (z1 + z2)


-- TODO: debug this.  Does this create raw structs?  Also test with unboxed vectors
main = print $ (Vec3 1 2 3) |+| (Vec3 1 2 3)

