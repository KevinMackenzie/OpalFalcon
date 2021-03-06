module OpalFalcon.Scene.Objects.DiscLight (
    DiscLight(MkDL),
    sampleDiscLight
    ) where

import OpalFalcon.BaseTypes
import OpalFalcon.Math.Ray
import OpalFalcon.Math.Vector
import OpalFalcon.Math.Matrix
import OpalFalcon.Math.Transformations
import OpalFalcon.Scene.Objects.PointLight
import OpalFalcon.Scene.Objects.Disc
import OpalFalcon.Scene.Objects.Plane

import Data.Bits

data DiscLight = MkDL Disc ColorRGBf Float

-- Generates points on an (sxs) square centered at the origin; TODO: use RNG
generateSquarePoints :: Double -> Integer -> [Vec2d]
generateSquarePoints s c = 
    let offset = s / (fromInteger c) 
        c2 = shiftR c 2
        cSet = map (\x -> offset * (1+2*(fromInteger x))) [-c2..(c2-1)]
    in  [V2 x y | x <- cSet, y <- cSet]

generateUnitDiscPoints :: Integer -> [Vec2d]
generateUnitDiscPoints c = filter (((>) 1.0) . mag) $ generateSquarePoints 2.0 c

getDiscTransform :: Disc -> Matrix4d
getDiscTransform (MkDisc (MkPlane space) _) = (translate (spacePos space)) ||*|| (lookAt $ zDir space)

transformDiscPoint :: Disc -> Vec2d -> Vec3d
transformDiscPoint d v = applyTransform3 (getDiscTransform d) $ promote3 v

generateDiscPoints :: Disc -> Integer -> [Vec3d]
generateDiscPoints d c = map (transformDiscPoint d) $ generateUnitDiscPoints c

-- TODO: Can we define a light source over an arbitrary surface if we can just define a function that generates points on the surface and treat each point as a point-light source.  Maybe we bundle points with colors so that each point light can have its own.

-- TODO: is this any faster than photon-mapping?  The simple image takes a while to render.

-- Instead of light sampling, is there any reason we couldn't do penumbra calculations analytically?

sampleDiscLight :: Integer -> DiscLight -> (Ray -> Maybe Hit) -> Vec3d -> ColorRGBf
sampleDiscLight c (MkDL lDisc lCol lPow) probe oPos = vecAverage $
    map (\x -> samplePointLight (MkPL x lCol lPow) probe oPos) $ generateDiscPoints lDisc c

