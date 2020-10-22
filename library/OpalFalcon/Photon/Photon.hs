{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
module OpalFalcon.Photon.Photon (Photon(..), estimateRadiance, estimateVolumeRadiance, mkPhoton, mkPhotonMap, PhotonMap(..), packDirFlags, unpackDirFlags, packColor, unpackColor) where

import OpalFalcon.BaseTypes
import OpalFalcon.Math.Vector
import OpalFalcon.Math.ConvexHull
import OpalFalcon.Photon.STHeap

import System.Random

import qualified Data.Vector.Storable as VS
import qualified GHC.Storable as GS
import qualified Foreign.Storable as FS
import Foreign.Ptr (castPtr)

import Debug.Trace
import Control.Monad
import Data.STRef
import qualified OpalFalcon.KdTree as Kd
import Data.Bits
import GHC.Exts
import GHC.Float (double2Float)
import GHC.Word (Word32)
import GHC.ST (ST(..), runST)
-- import OpalFalcon.Math.FastTrig

type PhotonMap = Kd.KdTree VS.Vector Photon

-- no-op
cullSphere _ _ _ _ _ = False

-- 'rPos' is the position in space; 'rSrcDir' is the direction the ray came from (flipped)
estimateVolumeRadiance :: PhotonMap -> Int -> Vec3d -> Vec3d -> Double -> PhaseFunc -> ColorRGBf
estimateVolumeRadiance pmap pCount rPos rSrcDir maxDist (PhaseFunc phase) = 
    let (photons, _) = collectPhotons pmap pCount (\_ -> False) rPos maxDist
        pts = VS.fromList $ map (\(Photon pos _ _ _) -> pos) photons
        r = double2Float $ sqrt $ VS.foldl max 0 $ VS.map (distance2 rPos) pts
        volume = if VS.length pts == 0 then Nothing else Just $ (4/3)*pi*r*r*r
        -- TODO: This looks more wrong with the convex hull alg (negative volumes :/)
        -- volume0 = double2Float <$> (convexHull3DVolume pts)
        col = case volume of 
            Nothing -> black
            Just v -> (1/v) *| (foldl (\c (Photon _ pow inc _) -> (pow |*| (phase inc rSrcDir)) |+| c) black photons)
     in col

cullCylinder:: Vec3d -> Double -> Double -> Vec3d -> Vec3d -> Bool
-- cullCylinder _ _ _ _ _ = False
cullCylinder dir r h pos x 
    | (distance2 pos x) > (r*r) = True
    | (abs $ (x |-| pos) |.| dir) > h = True
    | otherwise = False

-- Gets the estimated radiance at a certain point in the photon map with a
--  known surface normal, using a certain number of photons and only searching
--  up to a maximum distance before giving up. 'incdir' is flipped
estimateRadiance:: PhotonMap -> Int -> Vec3d -> Vec3d -> Double -> Bssrdf -> Vec3d -> ColorRGBf
estimateRadiance pmap pCount hpos incDir maxDist (Bssrdf bssrdf) norm = 
    let (photons, cnt) = collectPhotons pmap pCount (cullCylinder norm maxDist (0.001*maxDist) hpos) hpos maxDist
        pts = map (\(Photon pos _ _ _) -> pos) photons
        -- r2 = double2Float $ foldl max 0 $ map (distance2 hpos) pts
        -- area = if r2 == 0 then Nothing else Just $ pi*r2
        area = ((\x -> if x == 0 then trace ("Hit at (" ++ (show hpos) ++ ") wiht norm (" ++ (show norm) ++ ") and " ++ (show $ length pts) ++ " photons") x else x) . double2Float) <$> convexHull2DArea pts norm
        r = case area of
            Nothing -> black -- No area means no photons or not enough
            Just a -> (1/a) *| (foldl (\c (Photon pos pow inc _) -> (pow |*| (bssrdf (pos, inc) (hpos, incDir))) |+| c) black photons)
            -- If there are fewer than a certain percent of photons, try to avoid artifacts (questionable)
      in {-if ((fromIntegral (length pts)) < 0.05*(fromIntegral pCount)) then black else-} r


mkPhotonMap :: [Photon] -> PhotonMap
mkPhotonMap = Kd.mkKdTree

-- A photon inside of the photon acceleration structure
--                    pos   col       inc  flags
data Photon = Photon Vec3d ColorRGBf Vec3d Kd.KdAxis deriving (Show, Read)

mkPhoton :: Vec3d -> ColorRGBf -> Vec3d -> Photon
mkPhoton h c d = Photon h c d Kd.XAxis

indexPhotonMap :: PhotonMap -> Int -> Photon
indexPhotonMap (Kd.KdTree pmap) i = pmap `VS.unsafeIndex` i

-- TODO: we will want an option that lets us get a radiance estimate without converting to a list
collectPhotons :: PhotonMap -> Int -> (Vec3d -> Bool) -> Vec3d -> Double -> ([Photon],Int)
collectPhotons pmap n cull x maxDist = 
    let pmapSize = Kd.kdTreeSize pmap
        recurse p heap d2 = 
            let ph@(Photon pos _ _ axis) = indexPhotonMap pmap p
                l = recurse (2*p) heap d2
                r = recurse (2*p+1) heap d2
                del = (Kd.axisElem axis x) - (Kd.axisElem axis pos)
                del2 = distance2 x pos
            in do
                when (2*p+1 < pmapSize) $ do
                    if del < 0
                        then do l;r
                        else do r;l
                d2Val <- readSTRef d2
                when (del2 < d2Val && (not $ cull pos)) $ do
                    pushHeap ph heap
                    hSize <- getHeapSize heap
                    when (hSize == n) $ do
                        (Photon rp _ _ _) <- getHeapRoot heap
                        writeSTRef d2 $ distance2 rp x
     in runST $ do
        heap <- mkSTHeap n (Photon origin origin origin Kd.XAxis) (\(Photon x' _ _ _) -> distance2 x x')
        d2 <- newSTRef (maxDist*maxDist)
        recurse 1 heap d2
        getHeapContents heap

-- struct photon {
--   float x,y,z;
--   char p[4] ; (r, g, b, pow)
--   char phi, theta;
--   short flag;
-- } sizeof(photon) = 20

genPhoton :: IO (Photon)
genPhoton = 
        let rp = randomIO :: IO Vec3d
            ri = randomIO :: IO Vec3d
            rc = randomIO :: IO ColorRGBf
        in  do {
            pos <- rp;
            inc <- ri;
            col <- rc;
            return $ Photon pos col inc Kd.XAxis
            }

genRandomPhotons :: Integer -> IO ([Photon])
genRandomPhotons n = if  n == 0 then return [] else do {
            ph <- genPhoton;
            t <- genRandomPhotons $ n - 1;
            return (ph:t) :: IO ([Photon])
            }


blankPhoton :: Photon
blankPhoton = Photon origin origin origin Kd.XAxis

instance Kd.KdTreeObject Photon where
    blank = blankPhoton
    pos (Photon p _ _ _) = p
    setAxis (Photon p c d _) ax = Photon p c d ax

{-# INLINE unpackFlags #-}
unpackFlags :: Word32 -> Kd.KdAxis
unpackFlags 1 = Kd.XAxis
unpackFlags 2 = Kd.YAxis
unpackFlags 3 = Kd.ZAxis
unpackFlags _ = error "Photon axis value violated"

{-# INLINE packFlags #-}
packFlags :: Kd.KdAxis -> Word32
packFlags Kd.XAxis = 1
packFlags Kd.YAxis = 2
packFlags Kd.ZAxis = 3
packFlags _ = error "Photon axis value violated"

-- This packing strategy could much more uniformly represent
--  possible directions (highly skewed in resolution near poles)
{-# INLINE packDir #-}
packDir :: Vec3d -> Word32
packDir (V3 x y z) = 
    let c2 = 256.0 / (2*pi)
        theta' = atan2 y x
        phi' = acos z
        theta = round $ c2 * theta' + 128
        phi = round $ 2*c2 * phi'
     in phi .|. (shiftL theta 8)

{-# INLINE unpackDir #-}
unpackDir :: Word32 -> Vec3d
unpackDir w =
    let c = pi / 256.0
        theta' = fromIntegral $ (shiftR w 8) .&. 0xff
        theta = 2*c*(theta'-128)
        phi' = fromIntegral $ w .&. 0xff
        phi = c*phi'
        x = (sin phi) * (cos theta)
        y = (sin phi) * (sin theta)
        z = cos phi
    in  V3 x y z

-- Hm: this is actually slower; maybe because It isn't smart enough to do the "C" thing and pack the data tightly and copy out only what it needs
-- {-# INLINE unpackDirApprox #-}
-- unpackDirApprox :: Word -> Vec3d
-- unpackDirApprox = fastLowPSphereToRect

-- Fast trig functions for 8 bit angle values
-- cosTable :: Data.Array Int Float
-- sinTable :: Data.Array Int Float
-- fastCos :: Word# -> Float
-- fastSin :: Word# -> Float
-- fastCos (W# w) = cosTable V.!! w
-- fastSin (W# w) = sinTable V.!! w

-- Converts between theta,phi direction portion of the 4th 32 bits to a normalized vec3 and converts the flags portion
{-# INLINE unpackDirFlags #-}
unpackDirFlags :: Word32 -> (Vec3d, Kd.KdAxis)
unpackDirFlags w = 
    let dir = w .&. 0xffff
        flags = shiftR (w .&. 0xffff0000) 16
    in  (unpackDir dir, unpackFlags flags)
{-# INLINE packDirFlags #-}
packDirFlags :: Vec3d -> Kd.KdAxis -> Word32
packDirFlags dir axis = (shiftL (packFlags axis) 16) .|. (packDir dir)
-- Converts between shared exponent color and separate exponent color
-- (see https://www.khronos.org/registry/OpenGL/extensions/EXT/EXT_texture_shared_exponent.txt)
{-# INLINE unpackColor #-}
unpackColor :: Word32 -> ColorRGBf
unpackColor w = 
    let n = 9 :: Int
        b = 15 :: Int
        col_p = fmap fromIntegral $ V3 (shiftR (w .&. 0x7ffffff) 18) (shiftR (w .&. 0x3ffff) 9) (w .&. 0x1ff)
        exp_p = fromIntegral $ shiftR w 27
     in fmap (\x -> (fromIntegral x) * 2^^(exp_p- b - n)) col_p
{-# INLINE packColor #-}
packColor :: ColorRGBf -> Word32
packColor col = 
    let n = 9 :: Int
        emax = 31 :: Int
        b = 15 :: Int
        sharedexp_max = (2^n-1)/(2^n) * (2^(emax-b))
        col_c = (fmap (\x -> max 0 $ min sharedexp_max x) col)
        max_c = foldl max 0 col_c
        exp_shared_p = (max (-b-1) $ floor (logBase 2 max_c)) + 1 + b
        max_s = floor $ max_c / (2^^(exp_shared_p - b - n)) + 0.5
        exp_shared = exp_shared_p + (if max_s < 2^n then 0 else 1)
        (V3 rs gs bs) = fmap (\x -> floor $ x / (2**(fromIntegral $ exp_shared - b - n)) + 0.5) col_c
    in  fromIntegral $ bs .|. (shiftL gs 9) .|. (shiftL rs 18) .|. (shiftL exp_shared 27)

instance FS.Storable Photon where
    {-# INLINE sizeOf #-}
    sizeOf _ = 20
    {-# INLINE alignment #-}
    alignment _ = 20
    {-# INLINE peekElemOff #-}
    peekElemOff = peekPhotonOffPtr
    {-# INLINE pokeElemOff #-}
    pokeElemOff = pokePhotonOffPtr

{-# INLINE peekPhotonOffPtr #-}
peekPhotonOffPtr :: Ptr Photon -> Int -> IO Photon
peekPhotonOffPtr p i =
    let readF x o = GS.readFloatOffPtr (castPtr p) (x*5 + o)
        readW x o = GS.readWord32OffPtr (castPtr p) (x*5 + o)
     in do 
         x <- readF i 0
         y <- readF i 1
         z <- readF i 2
         col <- readW i 3
         (dir, axis) <- unpackDirFlags <$> (readW i 4)
         return $ Photon (float2DoubleVec $ V3 x y z) (unpackColor col) dir axis
{-# INLINE pokePhotonOffPtr #-}
pokePhotonOffPtr :: Ptr Photon -> Int -> Photon -> IO ()
pokePhotonOffPtr p i (Photon (V3 x y z) col dir axis) =
    let writeF x o v = GS.writeFloatOffPtr (castPtr p) (x*5 + o) v
        writeW x o v = GS.writeWord32OffPtr (castPtr p) (x*5 + o) v
     in do
         writeF i 0 $ double2Float x
         writeF i 1 $ double2Float y
         writeF i 2 $ double2Float z
         writeW i 3 $ packColor col
         writeW i 4 $ packDirFlags dir axis
