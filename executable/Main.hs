module Main where

import OpalFalcon.Math.Vector
import OpalFalcon.Math.Ray
import OpalFalcon.Math.Transformations
import OpalFalcon.RayTracer
import OpalFalcon.Scene.ObjectCollections.ObjectList
import OpalFalcon.Scene.Objects
import OpalFalcon.Scene.Objects.Sphere
import OpalFalcon.Scene
import OpalFalcon.BaseTypes
import Data.Bits

import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as Bs
import Codec.Picture.Types
import Codec.Picture.Png

-- This is a test to see if we can get a simple rendering of a sphere
-- TODO: we need a way to save the results to a file (do ppm for now)

sph = mkSphereObject (origin) 2
ol :: ObjectList
ol = MkObjList { objList = [sph] }
sc = (MkScene {objects=ol})

genPixMap :: Integer -> Integer -> [(Integer, Integer)]
genPixMap x2 y2 = [(x,y) | x <- [-x2..(x2-1)],
                           y <- [-y2..(y2-1)] ]

-- Does ray-tracing with given camera position and direction on wxh pixels
doRt :: (ObjectCollection o) => Scene o -> Integer -> Integer -> Vec3d -> Vec3d -> Double -> [ColorRGBf]
doRt s w h cPos cDir fov = map (shootRay s) $ genRays w h cPos cDir fov

genRays w h cPos cDir fov = 
    let w2 = shiftR w 1
        h2 = shiftR h 1
        d = (1 / (tan fov)) * (fromInteger h2)
        pxMap = genPixMap w2 h2
        dirs = map (\(x,y) -> noHomo (normalize (
                                applyTransform (toHomoPos (normalize (V3 (fromInteger x) (fromInteger y) (-d)))) 
                                               (lookAt cDir)
                              ))) pxMap
    in  map (MkRay cPos) dirs

printHits hs = foldl (++) "\n" $ map (\x -> case x of Nothing -> ""
                                                      Just h -> show h) hs

saveToPng :: String -> [ColorRGBf] -> Int -> Int -> IO ()
saveToPng p l w h = 
    let pixs = V.fromList $ map (((\(V3 x y z) -> PixelRGB8 x y z) . toPixel) :: (ColorRGBf -> PixelRGB8)) l
        bs = encodePng $ generateImage (\x y -> pixs V.! (x+y*w)) w h
    in do Bs.writeFile p bs

main :: IO ()
main = let pixs = doRt sc 100 100 (V3 0 0 10) (V3 0 0 (-1)) 75.0
           -- grads = (map (\(x,y) -> V3 (fromInteger x) (fromInteger y) 0.0) (genPixMap 25 25))
           -- hits = debugHits 0 sc 25 25 (V3 0 0 10) (V3 0 0 (-1)) 75.0
           -- r0 = defaultRtRay $ MkRay (V3 0 0 10) (V3 0 0 (-1))
           -- h0 = probeCollection ol $ rayBase r0
           -- r1 = fmap (deriveRay r0) h0
           -- h1 = fmap ((probeCollection ol) . rayBase) r1
       in  do {
           -- writeFile "outfile.ppm" $ encodePpm pixs 100 100;
           saveToPng "pngfile.png" pixs 100 100;
           -- writeFile "hittests.txt" $ printHits hits
           -- print $ reflect (V3 0 0 1) (normalize (V3 0 1 1));
           -- print $ reflectRay (MkRay (V3 0 0 1) (V3 0 0 (-1))) (normalize (V3 1 1 1));
           -- print $ closerPos (V3 0 0 0) (Just (V3 1 2 1)) (Just (V3 2 1 2));
           -- print $ hittestSphere (MkSphere (V3 0 0 (-1)) 2) (MkRay (V3 0 0 10) (V3 0 0 (-1)));
           -- print $ objIntersectRay sph (MkRay (V3 0 0 10) (V3 0 0 (-1)));
           }

-- TODO: uhh, write some fucking tests.  That's how you debug FP

