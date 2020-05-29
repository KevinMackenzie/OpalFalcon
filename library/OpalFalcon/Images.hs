module OpalFalcon.Images where

import OpalFalcon.Math.Vector

import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as Bs
import Codec.Picture.Types
import Codec.Picture.Png

toPixel :: ColorRGBf -> ColorRGB
toPixel = fmap ((min 255) . round . (255*) . linearToSRGB)

sRGBAlpha :: (Fractional a) => a
sRGBAlpha = 0.055
linearToSRGB x = if x <= 0.0031308 then 12.92*x else (1+sRGBAlpha)*((x ** (1/2.4))-sRGBAlpha)
sRGBToLinear x = if x <= 0.04045 then x/12.92 else ((x+sRGBAlpha)/(1+sRGBAlpha)) ** 2.4

saveToPngRtr :: String -> [ColorRGBf] -> Int -> Int -> IO ()
saveToPngRtr p l w h = 
    let pixs = V.fromList $ map (((\(V3 x y z) -> PixelRGB8 x y z) . toPixel) :: (ColorRGBf -> PixelRGB8)) l
        bs = encodePng $ generateImage (\x y -> pixs V.! ((h-1-y)+(w-1-x)*h)) w h
    in do Bs.writeFile p bs


-- Requires row-major ordering...
saveToPngPmap :: String -> [ColorRGBf] -> Int -> Int -> IO ()
saveToPngPmap p l w h = 
    let pixs = V.fromList $ map (((\(V3 x y z) -> PixelRGB8 x y z) . toPixel) :: (ColorRGBf -> PixelRGB8)) l
        bs = encodePng $ generateImage (\x y -> pixs V.! (y*w+(w-1-x))) w h
    in do Bs.writeFile p bs
