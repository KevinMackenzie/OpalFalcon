module CornellBox where

import Control.Monad.Random
import qualified Data.Array as A
import qualified Data.Vector as VB
import qualified Data.Vector.Storable as VS
import Debug.Trace
import OpalFalcon.BaseTypes
import qualified OpalFalcon.Geometry.Extrusions as Ext
import OpalFalcon.Images
import qualified OpalFalcon.KdTree as Kd
import OpalFalcon.Material
import OpalFalcon.Math.Lighting
import qualified OpalFalcon.Math.MMesh as MM
import OpalFalcon.Math.Optics
import OpalFalcon.Math.Transformations
import qualified OpalFalcon.Math.TriMesh as TMesh
import OpalFalcon.Math.Vector
import OpalFalcon.Photon.Photon
import OpalFalcon.Photon.PhotonTracer
import OpalFalcon.Photon.Visualizer
import OpalFalcon.Scene
import OpalFalcon.Scene.Camera
import OpalFalcon.Scene.ObjectCollections.ObjectList
import OpalFalcon.Scene.Objects
import OpalFalcon.Scene.Objects.Disc
import OpalFalcon.Scene.Objects.Plane
import OpalFalcon.Scene.Objects.PointLight
import OpalFalcon.Scene.Objects.Sphere
import OpalFalcon.Scene.Objects.Triangle
import qualified OpalFalcon.Util.Misc as Misc
import OpalFalcon.Util.Random
import OpalFalcon.VolumeMaterial
import TestScene (pSphereMat)

cboxCam = Camera
  { cameraPos = V3 0 0 8,
    -- The 'x-dir' of this needs to be flipped for some reason
    cameraDir = normalize $ V3 0 0 (-1),
    cameraUp = V3 0 1 0,
    cameraFOV = 0.698, -- 40 degrees
    cameraAspect = 1
  }

-- The Cornell Box
scene :: Scene ObjectList
scene = MkScene
  { sceneObjects = MkObjList
      { objList = [left0, left1, right0, right1, top0, top1, bottom0, bottom1, back0, back1, light0, light1, pSphere, sphre] -- ++ qp0 ++ qp1
      },
    sceneCamera = cboxCam
  }
  where
    vlbf = V3 (-2) (-2) 2
    vlbb = V3 (-2) (-2) (-2)
    vltf = V3 (-2) 2 2
    vltb = V3 (-2) 2 (-2)
    vrbf = V3 2 (-2) 2
    vrbb = V3 2 (-2) (-2)
    vrtf = V3 2 2 2
    vrtb = V3 2 2 (-2)
    llf = V3 (-0.5) 1.999 0.5
    llb = V3 (-0.5) 1.999 (-0.5)
    lrf = V3 0.5 1.999 0.5
    lrb = V3 0.5 1.999 (-0.5)
    diffT d t _ = mkDiffuseMat d (triangleNorm t)
    diffM d = (\m idx _ -> mkDiffuseMat d (TMesh.triNorm m idx))
    leftMat = diffT $ 0.84 *| ((V3 170 70 70) |/ 255)
    rightMat = diffT $ 0.84 *| ((V3 68 93 156) |/ 255)
    greenMat = diffT (V3 0 1 0)
    blankMat = diffT (V3 0.84 0.84 0.84)
    lightMat = diffT (V3 0 0 0)
    reflMat d t _ = mkSpecularMat d (triangleNorm t)
    boxMat = reflMat (V3 0.7 0.7 1)
    left0 = mkTriangleObject (MkTriangle vlbf vlbb vltb) leftMat
    left1 = mkTriangleObject (MkTriangle vlbf vltb vltf) leftMat
    right0 = mkTriangleObject (MkTriangle vrbf vrtb vrbb) rightMat
    right1 = mkTriangleObject (MkTriangle vrbf vrtf vrtb) rightMat
    top0 = mkTriangleObject (MkTriangle vrtf vltf vltb) blankMat
    top1 = mkTriangleObject (MkTriangle vrtf vltb vrtb) blankMat
    bottom0 = mkTriangleObject (MkTriangle vrbf vrbb vlbb) blankMat
    bottom1 = mkTriangleObject (MkTriangle vrbf vlbb vlbf) blankMat
    back0 = mkTriangleObject (MkTriangle vrtb vltb vlbb) blankMat
    back1 = mkTriangleObject (MkTriangle vrtb vlbb vrbb) blankMat
    light0Tri = MkTriangle lrb lrf llf
    light1Tri = MkTriangle llf llb lrb
    light0 = mkTriangleObjectFull light0Tri lightMat whitef 12
    light1 = mkTriangleObjectFull light1Tri lightMat whitef 12
    mesh = mkTriMeshObject mesh0 (diffM $ gray 0.3) -- (pMeshMat (\_ -> constVec 0.2) (\_ -> constVec 1.0) (PhaseFunc (\_ _ -> whitef |/ (4 * pi))))
    sphre = mkSphereObject (MkSphere (mkAffineSpace (V3 (-0.9) (-1.25) (-0.9)) xAxis yAxis zAxis) 0.75) (sphereMat (V3 0.9 0.9 0.6))
    pSphere = mkSphereObject (MkSphere (mkAffineSpace (V3 0.9 (-1.25) 0.7) xAxis yAxis zAxis) 0.75) (sSphMat (\_ -> constVec 0.05) (\_ -> constVec 10.0) (PhaseFunc (\_ _ -> whitef |/ (4 * pi))) 1.5)
    qp0 = Ext.mkQuadPrism (V3 (-0.5) (-1) (-0.6)) (normalize $ V3 3 0 (-1)) yAxis (normalize $ V3 1 0 3) (V3 0.5 1 0.5) boxMat
    qp1 = Ext.mkQuadPrism (V3 1 (-1.5) 1) (normalize $ V3 3 0 1) yAxis (normalize $ V3 (-1) 0 3) (V3 0.75 0.5 0.75) blankMat

sphereMat refl s hp = mkSpecularMat refl $ sphereNorm s hp

mesh0 =
  TMesh.new
    ( VS.fromList
        [ V3 1 (-1.99) (-1), -- back bottom left 0
          V3 1.99 (-1.99) (-1), -- back bottom right 1
          V3 1.99 (-1.99) 1, -- front bottom right 2
          V3 1 (-1.99) 1, -- front bottom left 3
          V3 1 (-0.7) (-1), -- back top left 4
          V3 1.99 (-0.7) (-1), -- back top right 5
          V3 1.99 (-0.7) 1, -- front top rigth 6
          V3 1 (-0.7) 1, -- front top left 7
          V3 1 (-0.5) (-0.7), -- Ridges 8
          V3 1 (-0.7) (-0.4), -- 9
          V3 1 (-0.5) (-0.1), -- 10
          V3 1 (-0.7) 0.2, -- 11
          V3 1 (-0.5) 0.5, -- 12
          V3 1 (-0.7) 0.8, -- 13
          V3 1 (-0.5) 1.0, -- 14
          V3 1.99 (-0.7) (-0.7), -- 15
          V3 1.99 (-0.7) (-0.4), -- 16
          V3 1.99 (-0.7) (-0.1), -- 17
          V3 1.99 (-0.7) 0.2, -- 18
          V3 1.99 (-0.7) 0.5, -- 19
          V3 1.99 (-0.7) 0.8 -- 20
        ]
    )
    ( VB.fromList
        [ MM.mkTri 0 2 1, -- Bottom tri 0
          MM.mkTri 0 3 2, -- Bottom tri 1
          MM.mkTri 7 3 2, -- Front tri 0
          MM.mkTri 7 2 6, -- Front tri 1
          MM.mkTri 6 2 1, -- Right tri 0
          MM.mkTri 6 1 5, -- Right tri 1
          MM.mkTri 5 1 0, -- Back tri 0
          MM.mkTri 5 0 4, -- Back tri 1
          MM.mkTri 4 0 3, -- Left tri 0
          MM.mkTri 4 3 7, -- Left tri 1
          MM.mkTri 4 9 8, -- Ridge tri 0
          MM.mkTri 9 11 10, -- Ridge Tri 1
          MM.mkTri 11 13 12, -- Ridge Tri 2
          MM.mkTri 13 7 14, -- Ridge Tri 3
          MM.mkTri 7 6 14, -- Front tri 3
          MM.mkTri 14 6 13, -- Top Ridge 0 0
          MM.mkTri 13 6 20, -- Top Ridge 0 1
          MM.mkTri 13 20 12, -- Top Ridge 1 0
          MM.mkTri 12 20 19, -- Top Ridge 1 1
          MM.mkTri 12 19 11, -- Top Ridge 2 0
          MM.mkTri 11 19 18, -- Top Ridge 2 1
          MM.mkTri 11 18 10, -- Top Ridge 3 1
          MM.mkTri 10 18 17, -- Top Ridge 3 1
          MM.mkTri 10 17 9, -- Top Ridge 4 1
          MM.mkTri 9 17 16, -- Top Ridge 4 1
          MM.mkTri 9 16 8, -- Top Ridge 5 1
          MM.mkTri 8 16 15, -- Top Ridge 5 1
          MM.mkTri 8 15 4, -- Top Ridge 6 1
          MM.mkTri 4 15 5 -- Top Ridge 6 1
        ]
    )

sSphMat :: (Vec3d -> ColorRGBf) -> (Vec3d -> ColorRGBf) -> PhaseFunc -> Double -> Sphere -> Vec3d -> AppliedMaterial
sSphMat absorb scatter phase ior sphere bc =
  mkScatteringMat
    ScatteringMaterial
      { scatteringParticipate = ParticipatingMaterial {participateAbsorb = absorb, participateScatter = scatter, participatePhase = phase, participateExit = hittestSphere sphere, participateImportance = (\_ -> getRandom)},
        scatteringRefract = ior
      }
    (sphereNorm sphere bc)
    bc
