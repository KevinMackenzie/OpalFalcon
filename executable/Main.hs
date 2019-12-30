{-# LANGUAGE BangPatterns #-}

module Main where

import qualified Data.Array as A
import OpalFalcon.BaseTypes
-- import OpalFalcon.RayTracer
import OpalFalcon.Images
import OpalFalcon.KdTree
import OpalFalcon.Material
import OpalFalcon.Math.Ray
import OpalFalcon.Math.Transformations
import OpalFalcon.Math.Vector
import OpalFalcon.PathTracer
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
import System.Random

-- The Cornell Box
cornellBox :: Scene ObjectList
cornellBox = MkScene
  { objects = MkObjList
      { objList = [left0, left1, right0, right1, top0, top1, bottom0, bottom1, back0, back1]
      },
    lightSources = []
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
    diffT d t _ = mkDiffuseMat d (normalize $ triangleNorm t)
    leftMat = diffT (V3 0.95 0.2 0.2)
    rightMat = diffT (V3 0.2 0.95 0.2)
    blankMat = diffT (V3 0.2 0.2 0.2)
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

sph :: Object

sph1 :: Object

ground :: Object

ol :: ObjectList

sc :: Scene ObjectList

sphereMat dif refl (MkSphere s _) hp = mkSimpleMat dif refl (normalize (hp |-| (affineTranslate s)))

planeMat dif (MkPlane (V4 _ _ n _)) _ = mkDiffuseMat dif (fromHomo n)

discMat dif (MkDisc (MkPlane (V4 _ _ n _)) _) _ = mkDiffuseMat (V3 0.5 0.8 0.4) (fromHomo n)

sph = mkSphereObject (MkSphere (mkAffineSpace (V3 (-2) 1.3 (-2)) xAxis yAxis zAxis) 1.5) (sphereMat (V3 0 1 0) (V3 0.8 0.4 0.4))

sph1 = mkSphereObject (MkSphere (mkAffineSpace (V3 2 1.3 (-2)) xAxis yAxis zAxis) 1.5) (sphereMat (V3 0 1 0) (V3 0.4 0.8 0.4))

ground = mkPlaneObject (MkPlane (mkAffineSpace (V3 0 (-2) 0) xAxis (negateVec zAxis) yAxis)) (planeMat (V3 0.5 0.5 0.5))

dPlane = MkPlane (mkAffineSpace (V3 (-2) 4 (-2)) xAxis zAxis (negateVec yAxis))

disc = mkDiscObject (MkDisc dPlane 2) (discMat (V3 0.4 0.6 0.6))

ol = MkObjList {objList = [sph, sph1, ground, disc]}

eol = MkObjList {objList = [disc]}

sc =
  ( MkScene
      { objects = ol,
        lightSources =
          [ -- mkPointLight (V3 (-4) 0 0) (V3 1 1 1) 10,
            -- mkPointLight (V3 6 0 2) (V3 1 1 1) 10,
            mkDiscLight (MkDisc dPlane 2) whitef 100
          ]
      }
  )

printHits hs =
  foldl (++) "\n" $
    map
      ( \x -> case x of
          Nothing -> ""
          Just h -> show h
      )
      hs

-- Generate a bunch of points on a sphere
spherePoints lonSteps latSteps radius pos =
  [ ( V3
        (radius * (cos theta) * (cos phi))
        (radius * (sin theta))
        (radius * (cos theta) * (sin phi))
    )
      |+| pos
    | theta <- [(n * pi / latSteps - pi / 2) | n <- [1 .. (latSteps -1)]],
      phi <- [(n * 2 * pi / lonSteps) | n <- [1 .. (lonSteps -1)]]
  ]

randomSpherePoints g =
  let (v, g') = randomUnitVec3 g
   in v : (randomSpherePoints g')

-- Filters the Nothing values out of a list
filterJust :: [Maybe a] -> [a]
filterJust [] = []
filterJust ((Nothing) : xs) = filterJust xs
filterJust ((Just x) : xs) = x : (filterJust xs)

spherePhotonShoot sc pow cnt pos = filterJust $ map (\(d, r) -> shootPhoton sc r (EPhoton (Ray pos d) pow)) $ zip (take cnt $ randomSpherePoints $ mkStdGen 0xdeadbeef) $ randGens $ mkStdGen 0x1337dead

main :: IO ()
main =
  let w = 500
      h = 500
      -- pixs = pathTraceScene (mkStdGen 0x1337dead) sc w h (Ray (V3 0 1 5) (normalize $ V3 0 (-0.2) (-1))) 90.0
      -- tph = shootPhoton cornellBox (mkStdGen 0xdeadbeef) (EPhoton (Ray (V3 0 0 0) (normalize $ V3 0.5 0.5 (-1))) whitef)
      phs = spherePhotonShoot cornellBox (constVec 0.05) 300000 (V3 0 0 0)
      cam = Camera {cameraPos = V3 0 0 7, cameraDir = V3 0 0 (-1), cameraUp = V3 0 1 0, cameraFOV = 90, cameraAspect = 1}
      fb = renderPhotons cornellBox cam h phs
   in do
        -- saveToPngRtr "pngfile.png" pixs w h;
        -- putStr $ foldl (\x y -> x ++ "\n" ++ (show y)) "" phs;
        saveToPngPmap "pmap.png" (fbPixelList fb) (fbWidth fb) (fbHeight fb)
