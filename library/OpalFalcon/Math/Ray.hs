module OpalFalcon.Math.Ray where

import OpalFalcon.Math.Vector

data Ray = Ray Vec3d Vec3d
    deriving (Eq, Show)

pointAtParameter :: Ray -> Double -> Vec3d
pointAtParameter (Ray rPos rDir) p = rPos |+| (p *| rDir)

advanceRay :: Ray -> Double -> Ray
advanceRay r@(Ray _ d) p = Ray (pointAtParameter r p) d

reflectRay :: Ray -> Vec3d -> Vec3d
reflectRay (Ray _ d) norm = reflect (negateVec d) norm

