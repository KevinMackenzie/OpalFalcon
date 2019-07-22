module OpalFalcon.Scene where

import qualified OpalFalcon.BaseTypes as Bt
import qualified OpalFalcon.Math.Ray as R

class ObjectCollection c where
    probeCollection :: c -> R.Ray -> Maybe Bt.Hit

data Scene o = MkScene { objects :: o }

-- instance (ObjectCollection a) => Scene a where
    --probeCollection s = probeCollection (objects s)

