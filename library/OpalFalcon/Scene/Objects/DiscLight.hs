module OpalFalcon.Scene.Objects.DiscLight where

import OpalFalcon.BaseTypes
import OpalFalcon.Math.Ray
import OpalFalcon.Math.Vector
import OpalFalcon.Scene.Objects.PointLight
import OpalFalcon.Scene.Objects.Disc

data DiscLight = MkDL Disc ColorRGBf Float

-- TODO: Can we define a light source over an arbitrary surface if we can just define a function that generates points on the surface and treat each point as a point-light source.  Maybe we bundle points with colors so that each point light can have its own.

-- Instead of light sampling, is there any reason we couldn't do penumbra calculations analytically?

sampleDiscLight :: Integer -> DiscLight -> (Ray -> Maybe Hit) -> Vec3d -> ColorRGBf
sampleDiscLight c (MkDL lDisc lCol lPow) probe oPos = vecAverage $
    map (\x -> samplePointLight (MkPL x lCol lPow) probe oPos) $ getDiscPoints lDisc c

