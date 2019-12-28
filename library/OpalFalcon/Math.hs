module OpalFalcon.Math where

deg2Rad :: Floating a => a -> a
deg2Rad d = (d * pi) / 180

rad2Deg :: Floating a => a -> a
rad2Deg r = (r * 180) / pi
