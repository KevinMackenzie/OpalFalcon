{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

-- import OpalFalcon.RayTracer

import Control.Monad.Random
import qualified Data.Array as A
import Debug.Trace
import OpalFalcon.BaseTypes
import OpalFalcon.Images
import OpalFalcon.KdTree
import OpalFalcon.Material
import OpalFalcon.Math.Lighting
import OpalFalcon.Math.Ray
import OpalFalcon.Math.Transformations
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
import OpalFalcon.Util.Random
-- import OpalFalcon.XTracing.PathTracer
import OpalFalcon.XTracing.RayTracer
import OpalFalcon.XTracing.XTracer
import System.Random
import System.Environment

mkQuadPrism pos xDir yDir zDir (V3 sx sy sz) mat =
  let pts@[lbb, lbf, ltb, ltf, rbb, rbf, rtb, rtf] =
        [ (pos |+| (xDir |* x) |+| (yDir |* y) |+| (zDir |* z))
          | x <- [- sx, sx],
            y <- [- sy, sy],
            z <- [- sz, sz]
        ]
      triples = [(lbb, lbf, ltb), (lbf, ltf, ltb), (ltf, lbf, rbf), (ltf, rbf, rtf), (rtf, rbf, rbb), (rtf, rbb, rtb), (rtb, rbb, lbb), (rtb, lbb, ltb), (ltf, rtf, rtb), (ltf, rtb, ltb), (lbf, rbb, rbf), (lbf, lbb, rbb)]
   in map (\(a, b, c) -> mkTriangleObject (MkTriangle a b c) mat) triples

-- The Cornell Box
cornellBox :: Scene ObjectList
cornellBox = MkScene
  { objects = MkObjList
      { objList = [left0, left1, right0, right1, top0, top1, bottom0, bottom1, back0, back1, light0, light1] ++ qp0 ++ qp1
      },
    lightSources =
      [ --mkDiscLight (MkDisc dPlane 0.75) whitef 25
        mkTriangleLight light0Tri whitef 25,
        mkTriangleLight light1Tri whitef 25
      ]
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
    diffT d t _ = mkDiffuseMat d (normalize $ triangleNorm t)
    leftMat = diffT (V3 1 0 0)
    rightMat = diffT (V3 0 0 1)
    blankMat = diffT (V3 0.84 0.84 0.84)
    lightMat = diffT (V3 0 0 0)
    reflMat d t _ = mkSpecularMat d (normalize $ triangleNorm t)
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
    light0 = mkTriangleObject light0Tri lightMat
    light1 = mkTriangleObject light1Tri lightMat
    sphre = mkSphereObject (MkSphere (mkAffineSpace (V3 0.3 (-1.25) 0) xAxis yAxis zAxis) 0.75) (sphereMat (V3 0.9 0.9 0.6))
    qp0 = mkQuadPrism (V3 (-0.5) (-1) (-0.6)) (normalize $ V3 3 0 (-1)) yAxis (normalize $ V3 1 0 3) (V3 0.5 1 0.5) boxMat
    qp1 = mkQuadPrism (V3 1 (-1.5) 1) (normalize $ V3 3 0 1) yAxis (normalize $ V3 (-1) 0 3) (V3 0.75 0.5 0.75) blankMat

sph :: Object

sph1 :: Object

ground :: Object

ol :: ObjectList

sc :: Scene ObjectList

sphereMat refl (MkSphere s _) hp = mkSpecularMat refl (normalize (hp |-| (affineTranslate s)))

planeMat refl (MkPlane (V4 _ _ n _)) _ = mkDiffuseMat refl (fromHomo n)

discMat dif (MkDisc (MkPlane (V4 _ _ n _)) _) _ = mkDiffuseMat (V3 0.5 0.8 0.4) (fromHomo n)

sph = mkSphereObject (MkSphere (mkAffineSpace (V3 (-2) 1.3 (-2)) xAxis yAxis zAxis) 1.5) (sphereMat (V3 0.8 0.4 0.4))

sph1 = mkSphereObject (MkSphere (mkAffineSpace (V3 2 1.3 (-2)) xAxis yAxis zAxis) 1.5) (sphereMat (V3 0.4 0.8 0.4))

ground = mkPlaneObject (MkPlane (mkAffineSpace (V3 0 (-2) 0) xAxis (negateVec zAxis) yAxis)) (planeMat (V3 0.5 0.5 0.5))

dPlane = MkPlane (mkAffineSpace (V3 0 1.998 0) xAxis zAxis (negateVec yAxis))

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

-- Filters the Nothing values out of a list
filterJust :: [Maybe a] -> [a]
filterJust [] = []
filterJust ((Nothing) : xs) = filterJust xs
filterJust ((Just x) : xs) = x : (filterJust xs)

spherePhotonShoot sc pow cnt pos =
  do
    rnd <- getRandoms
    concat <$> (mapM (\d -> shootPhoton 0 sc (EPhoton (Ray pos d) (pow |/ (fromIntegral cnt)))) $ take cnt rnd)

repeatMF :: (Monad m) => Integer -> m a -> m [a]
repeatMF 0 f = return []
repeatMF c f = do
  v <- f
  (v :) <$> (repeatMF (c -1) f)

areaPhotonShoot :: (Monad m, RandomGen g, ObjectCollection o) => Scene o -> ColorRGBf -> Int -> Vec3d -> Vec3d -> Vec3d -> Int -> RandT g m [Photon]
areaPhotonShoot sc pow cnt pos x2 y2 minBounce =
  let norm = normalize $ x2 |><| y2
   in do
        dirs <- repeatMF (toInteger cnt) $ cosWeightedDir norm
        xs <- (fmap (\a -> (2 * a -1) *| x2)) <$> getRandoms
        ys <- (fmap (\a -> (2 * a -1) *| y2)) <$> getRandoms
        concat <$> (mapM (\((d, x, y) :: (Vec3d, Vec3d, Vec3d)) -> shootPhoton minBounce sc (EPhoton (Ray (pos |+| x |+| y) d) (pow |/ (fromIntegral cnt)))) $ take cnt $ zip3 dirs xs ys)

main :: IO ()
main =
  let w = 200
      h = 200
      -- pixs = pathTraceScene (globIllum pmap 200 1.0) (mkStdGen 0x1337beef) cornellBox cam h
      gil pmap pcount maxDist pos inc norm bssrdf = estimateRadiance pmap pcount pos inc maxDist bssrdf norm
      yesglil p = gil p 500 0.5
      noglil p _ _ _ _ = black
      -- ptrcr = PathTracer {globalIllum = gil pmap 200 1.0}
      -- pixs = pathTraceScene ptrcr cornellBox h cam
      -- tph = shootPhoton cornellBox (mkStdGen 0xdeadbeef) (EPhoton (Ray (V3 0 0 0) (normalize $ V3 0.5 0.5 (-1))) whitef)
      cam = Camera {cameraPos = V3 0 0 7, cameraDir = V3 0 0 (-1), cameraUp = V3 0 1 0, cameraFOV = 75, cameraAspect = 1}
      incand = (V3 255 214 170)
      wht = constVec 255
      lightPow = ((50 / 255) *| wht)
   in -- (phs',cnt) = collectPhotons pmap 30000 (V3 (-2) 0 0) 2
      -- (phs1,cnt1) = collectPhotons pmap 30000 (V3 0 0 (-2)) 2
      -- (phs2,cnt2) = collectPhotons pmap 30000 (V3 0 (-2) 0) 2
      -- fb = renderPhotons cornellBox cam h $ phs' ++ phs1 ++ phs2
      do
        args <- getArgs
        phs <- evalRandIO $ areaPhotonShoot cornellBox lightPow 100000 (V3 0 1.998 0) (V3 0.5 0 0) (V3 0 0 0.5) 1 -- only indirect lighting
        pmap <- return $ ((mkKdTree phs) :: PhotonMap)
        -- fb <- return $ renderPhotons cornellBox cam h phs
        -- pixs <- return $ renderIlluminance (gil pmap 100 0.5) cornellBox cam h
        -- print $ take 30 $ map (\(Photon pos col dir _) -> 100*|col) phs
        -- print $ length phs
        -- print $ length $ filter (\c -> (mag c) > 0) $ fbPixelList fb
        -- saveToPngPmap "pmap.png" ((100 *|) <$> (fbPixelList fb)) (fbWidth fb) (fbHeight fb)
        -- print $ splitList 10 [1..100]
        pixs <- rayTraceScene (read $ args !! 0) (RayTracer {globalIllum = yesglil pmap}) cornellBox h cam
        -- print $ length pixs
        saveToPngRtr "pngfile.png" pixs w h

-- TODO: list
-- Force stuff like luminance, radiance, etc. into type system to reduce mistakes; can we derive via?
--  X. Triangle-light w/ random sampling for shadows
--  2. support scattering in applied material (output dir & position)
--  3. Add simple participating media
--  4. Use shoot photons in parallel (i.e. worker threads take chunks of work); I HAVE ACCESS TO PARALLEL.ecse still
--  ?. Support refraction by only accepting back-side hits (assume closed triangle meshes)