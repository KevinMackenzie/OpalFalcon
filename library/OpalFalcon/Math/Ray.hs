module OpalFalcon.Math.Ray where

import OpalFalcon.Math.Vector

data Ray = MkRay Vec3d Vec3d
    deriving (Eq, Show)
pos :: Ray -> Vec3d
pos (MkRay p _) = p
dir :: Ray -> Vec3d
dir (MkRay _ d) = d

pointAtParameter :: Ray -> Double -> Vec3d
pointAtParameter (MkRay rPos rDir) p = rPos |+| (p *| rDir)

advanceRay :: Ray -> Double -> Ray
advanceRay r@(MkRay _ d) p = MkRay (pointAtParameter r p) d

reflectRay :: Ray -> Vec3d -> Vec3d
reflectRay r norm = reflect (negateVec $ dir r) norm

