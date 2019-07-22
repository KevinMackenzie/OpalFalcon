module Main where

import OpalFalcon.Math.Vector
import OpalFalcon.Math.Ray
import OpalFalcon.Math.Transformations
import OpalFalcon.RayTracer
import OpalFalcon.Scene.ObjectCollections.ObjectList
import OpalFalcon.Scene.Objects
import OpalFalcon.Scene
import Data.Bits

import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as Bs
import Codec.Picture.Types
import Codec.Picture.Png

-- This is a test to see if we can get a simple rendering of a sphere
-- TODO: we need a way to save the results to a file (do ppm for now)

ol :: ObjectList
ol = MkObjList { objList = [mkSphereObject (constVec 0) 2] }

genPixMap :: Integer -> Integer -> [(Integer, Integer)]
genPixMap x2 y2 = [(x,y) | x <- [-x2..(x2-1)],
                           y <- [-y2..(y2-1)] ]

-- Does ray-tracing with given camera position and direction on wxh pixels
doRt :: (ObjectCollection o) => Scene o -> Integer -> Integer -> Vec3d -> Vec3d -> Double -> [ColorRGBf]
doRt s w h cPos cDir fov = 
    let w2 = shiftR w 1
        h2 = shiftR h 1
        d = (1 / (tan fov)) * (fromInteger h2)
        pxMap = genPixMap w2 h2
        dirs = map (\(x,y) -> noHomo (normalize (
                                applyTransform (toHomoPos (normalize (V3 (fromInteger x) (fromInteger y) (-d)))) 
                                               (lookAt cDir)
                              ))) pxMap
        rays = map (MkRay cPos) dirs
    in  map (shootRay s) rays

ppmHeader :: Integer -> Integer -> String
ppmHeader w h = "P3\n" ++ (show w) ++ " " ++ (show h) ++ "\n" ++ (show (255 :: Integer)) ++ "\n"

ppmPixel :: ColorRGB -> String
ppmPixel c = foldr (\x y -> x ++ " " ++ y) "\n" $ fmap show c

encodePpm :: [ColorRGBf] -> Integer -> Integer -> String
encodePpm l w h = (ppmHeader w h) ++ (foldl (++) "" $ map (ppmPixel . toPixel) l)

saveToPng :: String -> [ColorRGBf] -> Int -> Int -> IO ()
saveToPng p l w h = 
    let pixs = V.fromList $ map (((\(V3 x y z) -> PixelRGB8 x y z) . toPixel) :: (ColorRGBf -> PixelRGB8)) l
        bs = encodePng $ generateImage (\x y -> pixs V.! (x+y*w)) w h
    in do Bs.writeFile p bs

main :: IO ()
main = let pixs = doRt (MkScene {objects=ol}) 50 50 (V3 0 0 10) (V3 0 0 (-1)) 75.0
           -- grads = (map (\(x,y) -> V3 (fromInteger x) (fromInteger y) 0.0) (genPixMap 25 25))
       in  do {
           writeFile "outfile.ppm" $ encodePpm pixs 50 50;
           saveToPng "pngfile.png" pixs 50 50
           }

-- TODO: something is going on where the rays aren't reflecting properly
--  and the absorbtion on the sphere is compounding to the bounce max every time it hits the sphere at all
--

