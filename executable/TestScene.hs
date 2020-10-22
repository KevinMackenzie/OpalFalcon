module TestScene where

import Control.Monad.Random
import qualified Data.Array as A
import qualified Data.Vector as VB
import qualified Data.Vector.Storable as VS
import Debug.Trace
import OpalFalcon.BaseTypes
import qualified OpalFalcon.Geometry.Curves as Curves
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

genQuad :: Vec3d -> Vec3d -> Vec3d -> Vec3d -> (VS.Vector Vec3d, VB.Vector MM.Tri)
genQuad p0 p1 p2 p3 =
  let pts = VS.fromList [p0, p1, p2, p3]
      tris = VB.fromList [MM.mkTri 0 1 2, MM.mkTri 0 2 3]
   in (pts, tris)

-- Generates a groomed region of snow with an open bottom plane
genGroomedCurve :: Vec3d -> Vec3d -> Vec3d -> Double -> Double -> VS.Vector Vec3d
genGroomedCurve origin forward right xPitch yPitch =
  let wDir = normalize right
      width = mag right
      yDir = normalize $ right |><| forward
      numPeaks = floor $ width / xPitch
      lowPoints =
        map
          ( \idx ->
              ((fromIntegral idx) * xPitch) *| wDir
          )
          [0 .. numPeaks]
      peakPoints =
        map
          ( \idx ->
              ((((fromIntegral idx) + 0.5) * xPitch) *| wDir) |+| (yPitch *| yDir)
          )
          [0 .. numPeaks -1]
   in VS.fromList $ map (|+| origin) $ Misc.interleave (lowPoints ++ [right]) peakPoints

getCamera :: Camera
getCamera =
  let cpos = V3 (-7) 3.5 2
      cdir = normalize $ (V3 0 0 (-1.5)) |-| cpos
      right = normalize $ cdir |><| (V3 0 1 0)
      cup = normalize $ right |><| cdir
   in Camera
        { cameraPos = cpos,
          -- The 'x-dir' of this needs to be flipped for some reason
          cameraDir = cdir,
          cameraUp = cup,
          cameraFOV = 0.61,
          cameraAspect = 1
        }

sceneLights :: [Object]
sceneLights =
  [light0, light1]
  where
    llf = V3 (-0.5) 3.999 0.5
    llb = V3 (-0.5) 3.999 (-0.5)
    lrf = V3 0.5 3.999 0.5
    lrb = V3 0.5 3.999 (-0.5)
    light0Tri = MkTriangle lrb lrf llf
    light1Tri = MkTriangle llf llb lrb
    lightMat = dTriMat (V3 0 0 0)
    light0 = mkTriangleObjectFull light0Tri lightMat whitef 25
    light1 = mkTriangleObjectFull light1Tri lightMat whitef 25

sceneMidWidth = 2

sceneBankWidth = 0.5

sceneLength = 3

ground =
  mkTriMeshObject
    ( uncurry TMesh.new $
        genQuad
          (V3 (-5) (-1.01) 2)
          (V3 5 (-1.01) 2)
          (V3 5 (-1.01) (-5))
          (V3 (-5) (-1.01) (-5))
    )
    (dMeshMat $ V3 0.8 0.8 0.8)

-- The simplest test scene
testScene0 :: Scene ObjectList
testScene0 = MkScene
  { sceneObjects = MkObjList
      { objList = [solid, ground] ++ sceneLights
      },
    sceneCamera = getCamera
  }
  where
    boundaryFunc x = - x + 0.3636 * x + 0.4
    -- TODO: Make this one polyline and extrude it all at once to get one solid volume
    boundaryLeft = genLeftBank boundaryFunc 1
    boundaryRight = genRightBank boundaryFunc 1
    groomed = VS.fromList $ [V3 (- sceneMidWidth / 2) 0 0, V3 (sceneMidWidth / 2) 0 0]
    -- groomed =
    --   genGroomedCurve
    --     (V3 (- sceneMidWidth / 2) 0 0)
    --     (V3 0 0 (- sceneLength))
    --     (V3 sceneMidWidth 0 0)
    --     0.0381
    --     0.0254
    bottomLeft = VS.fromList $ [V3 (- sceneMidWidth / 2 - sceneBankWidth) (-1) 0]
    bottomRight = VS.fromList $ [V3 (sceneMidWidth / 2 + sceneBankWidth) (-1) 0]
    line = VS.reverse $ VS.concat [bottomLeft, boundaryLeft, groomed, boundaryRight, bottomRight]
    solid =
      traceShow (VB.length $ TMesh.tris solidMesh) $
        mkTriMeshObject
          solidMesh
          --(dMeshMat $ V3 0.8 0.7 0.7)
          ( sMeshMat
              (\_ -> 0.05 *| whitef)
              (\_ -> 2.0 *| whitef)
              (PhaseFunc $ \_ _ -> whitef |/ (4 * pi))
              1
          )
    solidMesh = uncurry TMesh.new $ Ext.extrudePoly line (V3 0 0 (- sceneLength))

genBank :: (Double -> Double) -> Int -> VS.Vector Vec3d
genBank boundaryFunc subdiv =
  let points = Curves.linearInterpolant boundaryFunc (0, sceneBankWidth) subdiv
   in VS.fromList $ map (\(x, y) -> V3 x y 0) points

genRightBank :: (Double -> Double) -> Int -> VS.Vector Vec3d
genRightBank boundaryFunc subdiv =
  let bank = genBank boundaryFunc subdiv
   in VS.map (|+| (V3 (sceneMidWidth / 2) 0 0)) bank

genLeftBank :: (Double -> Double) -> Int -> VS.Vector Vec3d
genLeftBank boundaryFunc subdiv =
  let bank = genBank boundaryFunc subdiv
   in VS.map (\(V3 x y z) -> V3 (- x - sceneMidWidth / 2) y z) $ VS.reverse bank

-- The full complexity test scene (subject to change)
-- getTestScene0 :: Scene ObjectList
-- getTestScene0 = MkScene
--   { sceneObjects = MkObjList
--       { objList = [ground, groomed, boundaryLeft, boundaryRight] ++ sceneLights
--       },
--     sceneCamera = getCamera
--   }
--   where
--     planeMat d (MkPlane (V4 x y z _)) pos = mkDiffuseMat d (normalize $ (fromHomoDir x) |><| (fromHomoDir y))
--     groomed =
--       mkTriMeshObject
--         ( genGroomedSurface
--             (V3 (- sceneMidWidth / 2) 0 0)
--             (V3 0 0 (- sceneLength))
--             (V3 sceneMidWidth 0 0)
--             0.0381
--             0.0254
--         )
--         (dMeshMat $ V3 0.8 0.7 0.7)
--     boundaryFunc x = 0.4 * (1 / (10 * x + 1))
--     boundaryLeft = genLeftBank boundaryFunc 10
--     boundaryRight = genRightBank boundaryFunc 10

pSphereMat :: (Vec3d -> ColorRGBf) -> (Vec3d -> ColorRGBf) -> PhaseFunc -> Sphere -> Vec3d -> AppliedMaterial
pSphereMat absorb scatter phase s hp =
  mkVolumeMat
    ( ParticipatingMaterial
        { participateAbsorb = absorb,
          participateScatter = scatter,
          participatePhase = phase,
          participateExit = hittestSphereInside s,
          participateImportance = (\_ -> getRandom)
        }
    )
    hp

pMeshMat :: (Vec3d -> ColorRGBf) -> (Vec3d -> ColorRGBf) -> PhaseFunc -> TMesh.TriMesh -> Int -> Vec3d -> AppliedMaterial
pMeshMat absorb scatter phase mesh idx bc =
  mkVolumeMat
    ( ParticipatingMaterial
        { participateAbsorb = absorb,
          participateScatter = scatter,
          participatePhase = phase,
          participateExit = TMesh.hittestTriMeshInside mesh,
          participateImportance = (\_ -> getRandom)
        }
    )
    (TMesh.baryToWorld mesh idx bc)

sMeshMat :: (Vec3d -> ColorRGBf) -> (Vec3d -> ColorRGBf) -> PhaseFunc -> Double -> TMesh.TriMesh -> Int -> Vec3d -> AppliedMaterial
sMeshMat absorb scatter phase ior mesh idx bc =
  mkScatteringMat
    ScatteringMaterial
      { scatteringParticipate = ParticipatingMaterial
          { participateAbsorb = absorb,
            participateScatter = scatter,
            participatePhase = phase,
            participateExit = TMesh.hittestTriMeshInside mesh,
            participateImportance = (\_ -> getRandom)
          },
        scatteringRefract = ior
      }
    (TMesh.triNorm mesh idx)
    (TMesh.baryToWorld mesh idx bc)

dTriMat :: ColorRGBf -> Triangle -> Vec3d -> AppliedMaterial
dTriMat d t _ = mkDiffuseMat d (triangleNorm t)

dMeshMat :: ColorRGBf -> TMesh.TriMesh -> Int -> Vec3d -> AppliedMaterial
dMeshMat d m idx _ = mkDiffuseMat d (TMesh.triNorm m idx)

debugMeshMat :: TMesh.TriMesh -> Int -> Vec3d -> AppliedMaterial
debugMeshMat m idx bc = dMeshMat (double2FloatVec bc) m idx bc
