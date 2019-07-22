module OpalFalcon.Math.Ray where

import OpalFalcon.Math.Vector

data Ray = MkRay Vec3d Vec3d
pos :: Ray -> Vec3d
pos (MkRay p _) = p
dir :: Ray -> Vec3d
dir (MkRay _ d) = d

pointAtParameter :: Ray -> Double -> Vec3d
pointAtParameter (MkRay rPos rDir) p = rPos |+| (p *| rDir)

reflectRay :: Ray -> Vec3d -> Vec3d
reflectRay r norm = reflect (negateVec $ dir r) norm

