{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
module OpalFalcon.Photon.Photon where

import OpalFalcon.BaseTypes
import OpalFalcon.Math.Vector
import OpalFalcon.Photon.STHeap

import System.Random

import Debug.Trace
import Control.Monad
import Data.Array.ST
import Data.STRef
import OpalFalcon.KdTree
import Data.Array.Base
import Data.Bits
import GHC.Exts
import GHC.Float (double2Float)
import GHC.ST (ST(..), runST)
-- import OpalFalcon.Math.FastTrig

type PhotonMap = KdTree UArray Int Photon

-- no-op
cullSphere _ _ _ _ _ = False

-- 'rPos' is the position in space; 'rSrcDir' is the direction the ray came from (flipped)
estimateVolumeRadiance :: PhotonMap -> Int -> Vec3d -> Vec3d -> Double -> PhaseFunc -> ColorRGBf
estimateVolumeRadiance pmap pCount rPos rSrcDir maxDist (PhaseFunc phase) = 
    let (photons, _) = collectPhotons pmap pCount (\_ -> False) rPos maxDist
        pts = map (\(Photon pos _ _ _) -> pos) photons
        r = double2Float $ sqrt $ foldl max 0 $ map (distance2 rPos) pts
        volume = if r == 0 then Nothing else Just $ (4/3)*pi*r*r*r
        col = case volume of
            Nothing -> black
            Just v -> (1/v) *| (foldl (\c (Photon pos pow inc _) -> (pow |*| (phase inc rSrcDir)) |+| c) black photons)
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
estimateRadiance:: PhotonMap -> Int -> Vec3d -> Vec3d -> Double -> Brdf -> Vec3d -> ColorRGBf
estimateRadiance pmap pCount hpos incDir maxDist (Brdf brdf) norm = 
    let (photons, cnt) = collectPhotons pmap pCount (cullCylinder norm maxDist (0.001*maxDist) hpos) hpos maxDist
        pts = map (\(Photon pos _ _ _) -> pos) photons
        -- r2 = double2Float $ foldl max 0 $ map (distance2 hpos) pts
        -- area = if r2 == 0 then Nothing else Just $ pi*r2
        area = ((\x -> if x == 0 then trace ("Hit at (" ++ (show hpos) ++ ") wiht norm (" ++ (show norm) ++ ") and " ++ (show $ length pts) ++ " photons") x else x) . double2Float) <$> convexHullArea pts norm
        r = case area of
            Nothing -> black -- No area means no photons or not enough
            Just a -> (1/a) *| (foldl (\c (Photon pos pow inc _) -> (pow |*| (brdf inc incDir)) |+| c) black photons)
            -- If there are fewer than a certain percent of photons, try to avoid artifacts (questionable)
      in {-if ((fromIntegral (length pts)) < 0.05*(fromIntegral pCount)) then black else-} r


-- TODO: Theres an error in the convex hull code...
-- Finds the area of the convex hull containing all photons
convexHullArea :: [Vec3d] -> Vec3d -> Maybe Double
convexHullArea [] _ = Nothing -- Zero points: no area
convexHullArea (_:[]) _ = Nothing -- One point: no area
convexHullArea (_:(_:[])) _ = Nothing -- Two points: do something
convexHullArea pts norm = 
    let ((o,_):t) = convexHull pts norm
        -- Use the dot product to get the area projected onto the plane
        a2 = foldl (+) 0 $ map (\(p0, p1) -> norm |.| ((p0 |-| o) |><| (p1 |-| o))) t
      in Just $ a2 / 2

lstPairs' :: a -> [a] -> [(a,a)]
lstPairs' x (h:[]) = [(h,x)]
lstPairs' x (h:(t@(h':_))) = (h,h'):(lstPairs' x t)
-- Generates list of edges from an ordered list of vertices to form a closed polygon
lstPairs :: [a] -> [(a,a)]
lstPairs l@(h:_) = lstPairs' h l

insideEdge :: Vec3d -> Vec3d -> Vec3d -> Vec3d -> Bool
insideEdge p0 p1 pt norm = ((p1 |-| p0) |><| (pt |-| p0)) |.| norm >= 0
-- A set of functions that adds a point to a convex hull
removePoint :: [(Vec3d, Vec3d)] -> Vec3d -> Vec3d -> Vec3d -> [(Vec3d, Vec3d)]
removePoint [] p2 pt _ = [(pt,p2)]
removePoint ((pn, pn1):t) _ pt norm = 
    if insideEdge pn pn1 pt norm 
        then (pt,pn):((pn,pn1):t) -- because of convexity assumption
        else removePoint t pn1 pt norm
insertPoint' :: [(Vec3d, Vec3d)] -> Vec3d -> Vec3d -> [(Vec3d, Vec3d)]
insertPoint' [] _ _ = []
insertPoint' ((pn, pn1):t) pt norm = 
     if insideEdge pn pn1 pt norm 
        then (pn,pn1):(insertPoint' t pt norm) 
        else (pn,pt):(removePoint t pn1 pt norm)
-- The first point may be removed, so rotate the list until the first point won't be
-- TODO: this is awkward 
insertPoint :: [(Vec3d, Vec3d)] -> Vec3d -> Vec3d -> [(Vec3d, Vec3d)]
insertPoint [] _ _ = []
insertPoint h@((pn, pn1):t) pt norm =
    if insideEdge pn pn1 pt norm 
        then (pn, pn1):(insertPoint' t pt norm)
        else insertPoint (t ++ [(pn,pn1)]) pt norm

-- Finds the convex hull of a set of co-planar points
convexHull :: [Vec3d] -> Vec3d -> [(Vec3d, Vec3d)]
convexHull (p0:(p1:(p2:t))) norm =
    let (p1', p2') = if insideEdge p0 p1 p2 norm then (p1, p2) else (p2,p1)
        -- The initial convex hull (triangle)
        tr = lstPairs [p0, p1', p2']
     in foldl (\el pt -> insertPoint el pt norm) tr t

mkPhotonMap :: [Photon] -> PhotonMap
mkPhotonMap = mkKdTree

-- A photon that will evaluate its incoming direction strictly
-- #TODO: this will always be used in haskell code
--                    pos   col       inc  flags
data Photon = Photon Vec3d ColorRGBf Vec3d KdAxis deriving Show

mkPhoton :: Vec3d -> ColorRGBf -> Vec3d -> Photon
mkPhoton h c d = Photon h c d XAxis

indexPhotonMap :: PhotonMap -> Int -> Photon
indexPhotonMap (KdTree pmap) i = pmap ! i

-- TODO: we will want an option that lets us get a radiance estimate without converting to a list
collectPhotons :: PhotonMap -> Int -> (Vec3d -> Bool) -> Vec3d -> Double -> ([Photon],Int)
collectPhotons pmap n cull x maxDist = 
    let pmapSize = kdTreeSize pmap
        recurse p heap d2 = 
            let ph@(Photon pos _ _ axis) = indexPhotonMap pmap p
                l = recurse (2*p) heap d2
                r = recurse (2*p+1) heap d2
                del = (axisElem axis x) - (axisElem axis pos)
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
        heap <- mkSTHeap n (Photon origin origin origin XAxis) (\(Photon x' _ _ _) -> distance2 x x')
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

{-# INLINE unpackFlags #-}
unpackFlags :: Word -> KdAxis
unpackFlags 1 = XAxis
unpackFlags 2 = YAxis
unpackFlags 3 = ZAxis
unpackFlags _ = error "Photon axis value violated"

{-# INLINE packFlags #-}
packFlags :: KdAxis -> Word
packFlags XAxis = 1
packFlags YAxis = 2
packFlags ZAxis = 3
packFlags _ = error "Photon axis value violated"

{-# INLINE packDir #-}
packDir :: Vec3d -> Word
packDir (V3 x y z) = 
    let c2 = 256.0 / (2*pi)
        theta' = atan2 y x
        phi' = acos z
        theta = round $ c2 * theta' + 128
        phi = round $ 2*c2 * phi'
     in phi .|. (shiftL theta 8)

{-# INLINE unpackDir #-}
unpackDir :: Word -> Vec3d
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
unpackDirFlags :: Word -> (Vec3d, KdAxis)
unpackDirFlags w = 
    let dir = w .&. 0xffff
        flags = shiftR (w .&. 0xffff0000) 16
    in  (unpackDir dir, unpackFlags flags)
{-# INLINE packDirFlags #-}
packDirFlags :: Vec3d -> KdAxis -> Word
packDirFlags dir axis = (shiftL (packFlags axis) 16) .|. (packDir dir)
-- Converts between shared exponent color and separate exponent color
-- (see https://www.khronos.org/registry/OpenGL/extensions/EXT/EXT_texture_shared_exponent.txt)
{-# INLINE unpackColor #-}
unpackColor :: Word -> ColorRGBf
unpackColor w = 
    let n = 9 :: Int
        b = 15 :: Int
        col_p = fmap fromIntegral $ V3 (shiftR (w .&. 0x7ffffff) 18) (shiftR (w .&. 0x3ffff) 9) (w .&. 0x1ff)
        exp_p = fromIntegral $ shiftR w 27
     in fmap (\x -> (fromIntegral x) * 2^^(exp_p- b - n)) col_p
{-# INLINE packColor #-}
packColor :: ColorRGBf -> Word
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

