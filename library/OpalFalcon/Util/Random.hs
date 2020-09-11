module OpalFalcon.Util.Random where

import Control.Monad.Random

-- Generates an infinite list of distinct random number generators
randGens :: (RandomGen g) => g -> [g]
randGens g = g' : gs
  where
    (g', g'') = split g
    gs = randGens g''

-- Generates 0-free random floats
getRandomNonZero :: (Monad m, RandomGen g, Eq a, Random a, Floating a) => RandT g m a
getRandomNonZero =
  do
    f <- getRandom
    if f == 0 then getRandomNonZero else return f
