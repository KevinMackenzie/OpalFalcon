module OpalFalcon.Images where

import OpalFalcon.Math.Vector

import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as Bs
import Codec.Picture.Types
import Codec.Picture.Png

saveToPng :: String -> [ColorRGBf] -> Integer -> Integer -> IO ()
saveToPng p l w0 h0 = 
    let w = fromInteger w0
        h = fromInteger h0
        pixs = V.fromList $ map (((\(V3 x y z) -> PixelRGB8 x y z) . toPixel) :: (ColorRGBf -> PixelRGB8)) l
        bs = encodePng $ generateImage (\x y -> pixs V.! ((h-1-y)+(w-1-x)*h)) w h
    in do Bs.writeFile p bs

