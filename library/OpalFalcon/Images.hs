module OpalFalcon.Images where

import OpalFalcon.Math.Vector

import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as Bs
import Codec.Picture.Types
import Codec.Picture.Png

saveToPngRtr :: String -> [ColorRGBf] -> Int -> Int -> IO ()
saveToPngRtr p l w h = 
    let pixs = V.fromList $ map (((\(V3 x y z) -> PixelRGB8 x y z) . toPixel) :: (ColorRGBf -> PixelRGB8)) l
        bs = encodePng $ generateImage (\x y -> pixs V.! ((h-1-y)+(w-1-x)*h)) w h
    in do Bs.writeFile p bs


-- Requires row-major ordering...
saveToPngPmap :: String -> [ColorRGBf] -> Int -> Int -> IO ()
saveToPngPmap p l w h = 
    let pixs = V.fromList $ map (((\(V3 x y z) -> PixelRGB8 x y z) . toPixel) :: (ColorRGBf -> PixelRGB8)) l
        bs = encodePng $ generateImage (\x y -> pixs V.! ((h-1-y)*w+(x))) w h
    in do Bs.writeFile p bs
