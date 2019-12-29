{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
module OpalFalcon.Photon.Photon where

import OpalFalcon.Math.Vector

import System.Random

import OpalFalcon.KdTree
import Data.Array.Base
import Data.Bits
import GHC.Exts
import GHC.Word
import GHC.ST (ST(..), runST)

type PhotonMap = KdTree UArray Int Photon

-- Gets the estimated irradiance at a certain point in the photon map with a
--  known surface normal, using a certain number of photons and only searching
--  up to a maximum distance before giving up.
estimateIrradiance :: PhotonMap -> Vec3d -> Vec3d -> Double -> Integer -> ColorRGBf
estimateIrradiance pm pos norm maxDist pCount = whitef

mkPhotonMap :: [Photon] -> PhotonMap
mkPhotonMap = mkKdTree

-- A photon that will evaluate its incoming direction strictly
-- #TODO: this will always be used in haskell code
data Photon = Photon !Vec3d !ColorRGBf !Vec3d !KdAxis deriving Show

mkPhoton :: Vec3d -> ColorRGBf -> Vec3d -> Photon
mkPhoton h c d = Photon h c d XAxis

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
            return $ Photon pos col inc XAxis
            }

genRandomPhotons :: Integer -> IO ([Photon])
genRandomPhotons n = if  n == 0 then return [] else do {
            ph <- genPhoton;
            t <- genRandomPhotons $ n - 1;
            return (ph:t) :: IO ([Photon])
            }


blankPhoton :: Photon
blankPhoton = Photon origin origin origin XAxis

unpackFlags :: Word -> KdAxis
unpackFlags 1 = XAxis
unpackFlags 2 = YAxis
unpackFlags 3 = ZAxis
unpackFlags _ = error "Photon axis value violated"

packFlags :: KdAxis -> Word
packFlags XAxis = 1
packFlags YAxis = 2
packFlags ZAxis = 3
packFlags _ = error "Photon axis value violated"

packDir :: Vec3d -> Word
packDir (V3 x y z) = 
    let c2 = 256.0 / (2*pi)
        phi = round $ c2 * (atan2 y x)
        theta = round $ 2*c2*(acos z)
    in phi .|. (shiftL theta 8)

unpackDir :: Word -> Vec3d
unpackDir w =
    let c = pi / 256.0
        phi = fromInteger (toInteger (w .&. 0xff))
        theta = fromInteger (toInteger (shiftR (w .&. 0xff00) 8))
        x = 2*c*(cos phi)
        y = 2*c*(sin phi)
        z = c*(tan theta)
    in  V3 x y z

-- Fast trig functions for 8 bit angle values
-- cosTable :: Data.Array Int Float
-- sinTable :: Data.Array Int Float
-- fastCos :: Word# -> Float
-- fastSin :: Word# -> Float
-- fastCos (W# w) = cosTable V.!! w
-- fastSin (W# w) = sinTable V.!! w

-- Converts between theta,phi direction portion of the 4th 32 bits to a normalized vec3 and converts the flags portion
unpackDirFlags :: Word -> (Vec3d, KdAxis)
unpackDirFlags w = 
    let dir = w .&. 0xffff
        flags = shiftR (w .&. 0xffff0000) 16
    in  (unpackDir dir, unpackFlags flags)
packDirFlags :: Vec3d -> KdAxis -> Word
packDirFlags dir axis = (shiftL (packFlags axis) 16) .|. (packDir dir)
-- Converts between shared exponent color and separate exponent color
unpackColor :: Word -> ColorRGBf
unpackColor = undefined
packColor :: ColorRGBf -> Word
packColor = undefined

instance KdTreeObject Photon where
    blank = blankPhoton
    pos (Photon p _ _ _) = p
    setAxis (Photon p c d _) ax = Photon p c d ax

{-# INLINE indexPhotonArray# #-}
indexPhotonArray# :: ByteArray# -> Int# -> Photon
indexPhotonArray# arr# idx# = 
    let ifa# i# = F# (indexFloatArray# arr# (idx# *# 5# +# i#))
        iwa# i# = W# (indexWord32Array# arr# (idx# *# 5# +# i#))
        x = ifa# 0#
        y = ifa# 1#
        z = ifa# 2#
        col = iwa# 3#
        (dir, axis) = unpackDirFlags $ iwa# 4#
    in  Photon (float2DoubleVec $ V3 x y z)
               (unpackColor col)
               dir axis

{-# INLINE readPhotonArray# #-}
readPhotonArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Photon #) 
readPhotonArray# arr# idx# st# = 
    let offset = idx# *# 5#
        !(# s1#, x #) = readFloatArray# arr# (offset +# 0#) st#
        !(# s2#, y #) = readFloatArray# arr# (offset +# 1#) s1#
        !(# s3#, z #) = readFloatArray# arr# (offset +# 2#) s2#
        !(# s4#, col #) = readWord32Array# arr# (offset +# 3#) s3#
        !(# s5#, dirFlags #) = readWord32Array# arr# (offset +# 4#) s4#
        (dir, flags) = unpackDirFlags (W# dirFlags)
    in  (# s5#, Photon (float2DoubleVec $ V3 (F# x) (F# y) (F# z)) 
                       (unpackColor (W# col))
                       dir flags #)

{-# INLINE writePhotonArray# #-}
writePhotonArray# :: MutableByteArray# s -> Int# -> Photon -> State# s -> State# s
writePhotonArray# arr# idx# (Photon pos col dir flags) s0# =
    let !(V3 !(F# x) !(F# y) !(F# z)) = double2FloatVec pos
        offset = idx# *# 5#
        !(W# col#) = packColor col
        !(W# df) = packDirFlags dir flags
        s1# = writeFloatArray# arr# (offset +# 0#) x s0#
        s2# = writeFloatArray# arr# (offset +# 1#) y s1#
        s3# = writeFloatArray# arr# (offset +# 2#) z s2#
        s4# = writeWord32Array# arr# (offset +# 3#) col# s3#
        s5# = writeWord32Array# arr# (offset +# 4#) df s4#
    in  s5#

instance MArray (STUArray s) Photon (ST s) where
    {-# INLINE getBounds #-}
    getBounds (STUArray l u _ _) = return (l,u)
    {-# INLINE getNumElements #-}
    getNumElements (STUArray _ _ n _) = return n
    {-# INLINE unsafeNewArray_ #-}
    unsafeNewArray_ (l,u) = unsafeNewArraySTUArray_ (l,u) (safe_scale 24#)
    {-# INLINE newArray_ #-}
    newArray_ arrBounds = newArray arrBounds blankPhoton
    {-# INLINE unsafeRead #-}
    unsafeRead (STUArray _ _ _ marr#) (I# i#) = ST $ \s1# ->
        case readPhotonArray# marr# i# s1# of { (# s2#, e# #) ->
        (# s2#, e# #) }
    {-# INLINE unsafeWrite #-}
    unsafeWrite (STUArray _ _ _ marr#) (I# i#) e# = ST $ \s1# ->
        case writePhotonArray# marr# i# e# s1# of { s2# ->
        (# s2#, () #) }

instance IArray UArray Photon where
    {-# INLINE bounds #-}
    bounds (UArray l u _ _) = (l,u)
    {-# INLINE numElements #-}
    numElements (UArray _ _ n _) = n
    {-# INLINE unsafeArray #-}
    unsafeArray lu ies = runST (unsafeArrayUArray lu ies blankPhoton)
    {-# INLINE unsafeAt #-}
    unsafeAt (UArray _ _ _ arr#) (I# i#) = indexPhotonArray# arr# i#
    {-# INLINE unsafeReplace #-}
    unsafeReplace arr ies = runST (unsafeReplaceUArray arr ies)
    {-# INLINE unsafeAccum #-}
    unsafeAccum f arr ies = runST (unsafeAccumUArray f arr ies)
    {-# INLINE unsafeAccumArray #-}
    unsafeAccumArray f initialValue lu ies = runST (unsafeAccumArrayUArray f initialValue lu ies)

