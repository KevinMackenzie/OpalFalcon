module OpalFalcon.Util.Random where

import System.Random

-- Generates an infinite list of distinct random number generators
randGens :: (RandomGen g) => g -> [g]
randGens g = g' : gs
  where
    (g', g'') = split g
    gs = randGens g''
