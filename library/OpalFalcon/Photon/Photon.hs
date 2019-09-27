{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
module OpalFalcon.Photon.Photon where

import OpalFalcon.Math.Vector

import System.Random

import OpalFalcon.KdTree
import Data.Array.Base
import GHC.Exts
import Data.Bits ((.|.), (.&.), complement)
import GHC.ST (ST(..), runST)

import GHC.Float (float2Double)

type PhotonMap = KdTree UArray Int Photon

mkPhotonMap :: Int -> [Photon] -> PhotonMap
mkPhotonMap = mkKdTree

-- struct photon {
--   float x,y,z;
--   char p[4] ; (r, g, b, pow)
--   char phi, theta;
--   short flag;
-- } sizeof(photon) = 20
-- TODO: Its probably OK to use Vec3s on photons.  Its ok if the lifted type isn't as space efficient
--      as long as the array-form is since we'll need it in that form anyway and as long as its always strict
--                                   (x      y      z)  (rgb pow)  (phi   theta) flags
data Photon = Photon {-# UNPACK #-} !Float !Float !Float !Word !Word deriving Show

-- color :: Photon -> ColorRGBf
-- color = undefined -- TODO: convert between shared-power form and floats
-- 
-- incDir :: Photon -> (Word8, Word8)
-- incDir (Photon _ _ _ _ f) = ((shiftR 24 f) .&. 0xFF, (shiftR 16 f) .&. 0xFF)

genPhoton :: IO (Photon)
genPhoton = 
        let rf = randomIO :: IO Float
            rw = randomIO :: IO Word
        in  do {
            x <- rf;
            y <- rf;
            z <- rf;
            p <- rw;
            f <- rw;
            return $ Photon x y z (p .&. 0xFFFFFFFF) (f .&. 0xFFFFFFFF)
            }

genRandomPhotons :: Integer -> IO ([Photon])
genRandomPhotons n = if  n == 0 then return [] else do {
            ph <- genPhoton;
            t <- genRandomPhotons $ n - 1;
            return (ph:t) :: IO ([Photon])
            }


blankPhoton :: Photon
blankPhoton = Photon 0.0 0.0 0.0 0 0

v3ToDouble :: Vec3 Float -> Vec3d
v3ToDouble = fmap float2Double 

setRawAxis :: KdAxis -> Word -> Word
setRawAxis XAxis p = 1 .|. (p .&. (complement 7))
setRawAxis YAxis p = 2 .|. (p .&. (complement 7))
setRawAxis ZAxis p = 4 .|. (p .&. (complement 7))

getAxis :: Word -> KdAxis
getAxis p = case p .&. 7 of
    1 -> XAxis
    2 -> YAxis
    4 -> ZAxis
    _ -> error "Photon axis value violated"

instance KdTreeObject Photon where
    blank = blankPhoton
    pos (Photon x y z _ _) = v3ToDouble $ V3 x y z
    setAxis (Photon x y z p f) ax = Photon x y z p (setRawAxis ax f)

indexPhotonArray :: ByteArray# -> Int# -> Photon
indexPhotonArray arr# idx# = 
    Photon (ifa# 0#) (ifa# 1#) (ifa# 2#) (iwa# 3#) (iwa# 4#)
    where ifa# i# = F# (indexFloatArray# arr# (idx# *# 5# +# i#))
          iwa# i# = W# (indexWord32Array# arr# (idx# *# 5# +# i#))

readPhotonArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Photon #) 
readPhotonArray# arr# idx# st# = (# s5#, Photon (F# ifa0) (F# ifa1) (F# ifa2) (W# iwa3) (W# iwa4) #)
    where !(# s1#, ifa0 #) = readFloatArray# arr# (idx# *# 5# +# 0#) st#
          !(# s2#, ifa1 #) = readFloatArray# arr# (idx# *# 5# +# 1#) s1#
          !(# s3#, ifa2 #) = readFloatArray# arr# (idx# *# 5# +# 2#) s2#
          !(# s4#, iwa3 #) = readWord32Array# arr# (idx# *# 5# +# 3#) s3#
          !(# s5#, iwa4 #) = readWord32Array# arr# (idx# *# 5# +# 4#) s4#

writePhotonArray# :: MutableByteArray# s -> Int# -> Photon -> State# s -> State# s
writePhotonArray# arr# idx# (Photon (F# x) (F# y) (F# z) (W# p) (W# f)) s0# =
    let s1# = writeFloatArray# arr# (idx# *# 5# +# 0#) x s0#;
        s2# = writeFloatArray# arr# (idx# *# 5# +# 1#) y s1#;
        s3# = writeFloatArray# arr# (idx# *# 5# +# 2#) z s2#;
        s4# = writeWord32Array# arr# (idx# *# 5# +# 3#) p s3#;
        s5# = writeWord32Array# arr# (idx# *# 5# +# 4#) f s4#;
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
    unsafeAt (UArray _ _ _ arr#) (I# i#) = indexPhotonArray arr# i#
    {-# INLINE unsafeReplace #-}
    unsafeReplace arr ies = runST (unsafeReplaceUArray arr ies)
    {-# INLINE unsafeAccum #-}
    unsafeAccum f arr ies = runST (unsafeAccumUArray f arr ies)
    {-# INLINE unsafeAccumArray #-}
    unsafeAccumArray f initialValue lu ies = runST (unsafeAccumArrayUArray f initialValue lu ies)

