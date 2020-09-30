{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

-- import OpalFalcon.RayTracer

import Control.Monad.Random
import qualified Data.Array as A
import qualified Data.Vector as VB
import Debug.Trace
import OpalFalcon.BaseTypes
import OpalFalcon.Images
import qualified OpalFalcon.KdTree as Kd
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
import OpalFalcon.VolumeMaterial
-- import OpalFalcon.XTracing.PathTracer

import OpalFalcon.XTracing.RayTraceUtils
import OpalFalcon.XTracing.RayTracer
import OpalFalcon.XTracing.XTracer
import System.Directory
import System.Environment
import System.Process
import System.Random

mkQuadPrism pos xDir yDir zDir (V3 sx sy sz) mat =
  let pts@[lbb, lbf, ltb, ltf, rbb, rbf, rtb, rtf] =
        [ (pos |+| (xDir |* x) |+| (yDir |* y) |+| (zDir |* z))
          | x <- [- sx, sx],
            y <- [- sy, sy],
            z <- [- sz, sz]
        ]
      triples = [(lbb, lbf, ltb), (lbf, ltf, ltb), (ltf, lbf, rbf), (ltf, rbf, rtf), (rtf, rbf, rbb), (rtf, rbb, rtb), (rtb, rbb, lbb), (rtb, lbb, ltb), (ltf, rtf, rtb), (ltf, rtb, ltb), (lbf, rbb, rbf), (lbf, lbb, rbb)]
   in map (\(a, b, c) -> mkTriangleObject (MkTriangle a b c) mat) triples

emptyScene :: Scene ObjectList
emptyScene = MkScene
  { objects = MkObjList {objList = []},
    lightSources = []
  }

-- The Cornell Box
cornellBox :: Scene ObjectList
cornellBox = MkScene
  { objects = MkObjList
      { objList = [left0, left1, right0, right1, top0, top1, bottom0, bottom1, back0, back1, light0, light1, pSphere]
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
    greenMat = diffT (V3 0 1 0)
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
    back0 = mkTriangleObject (MkTriangle vrtb vltb vlbb) greenMat
    back1 = mkTriangleObject (MkTriangle vrtb vlbb vrbb) greenMat
    light0Tri = MkTriangle lrb lrf llf
    light1Tri = MkTriangle llf llb lrb
    light0 = mkTriangleObject light0Tri lightMat
    light1 = mkTriangleObject light1Tri lightMat
    sphre = mkSphereObject (MkSphere (mkAffineSpace (V3 0.3 (-1.25) 0) xAxis yAxis zAxis) 0.75) (sphereMat (V3 0.9 0.9 0.6))
    pSphere = mkSphereObject (MkSphere (mkAffineSpace (V3 0.3 (-1.25) 0) xAxis yAxis zAxis) 0.75) (pSphereMat (\_ -> constVec 0.2) (\_ -> constVec 0.6) (PhaseFunc (\_ _ -> whitef |/ (4 * pi))))
    qp0 = mkQuadPrism (V3 (-0.5) (-1) (-0.6)) (normalize $ V3 3 0 (-1)) yAxis (normalize $ V3 1 0 3) (V3 0.5 1 0.5) boxMat
    qp1 = mkQuadPrism (V3 1 (-1.5) 1) (normalize $ V3 3 0 1) yAxis (normalize $ V3 (-1) 0 3) (V3 0.75 0.5 0.75) blankMat

pSphereMat :: (Vec3d -> ColorRGBf) -> (Vec3d -> ColorRGBf) -> PhaseFunc -> Sphere -> Vec3d -> AppliedMaterial
pSphereMat absorb scatter phase s hp = mkVolumeMat (ParticipatingMaterial {participateAbsorb = absorb, participateScatter = scatter, participatePhase = phase, participateExit = exitSphere s}) hp

sphereMat refl (MkSphere s _) hp = mkSpecularMat refl (normalize (hp |-| (affineTranslate s)))

printHits hs =
  foldl (++) "\n" $
    map
      ( \x -> case x of
          Nothing -> ""
          Just h -> show h
      )
      hs

-- Filters the Nothing values out of a list
filterJust :: [Maybe a] -> [a]
filterJust [] = []
filterJust ((Nothing) : xs) = filterJust xs
filterJust ((Just x) : xs) = x : (filterJust xs)

repeatMF :: (Monad m) => Integer -> m a -> m [a]
repeatMF 0 f = return []
repeatMF c f = do
  v <- f
  (v :) <$> (repeatMF (c -1) f)

areaPhotonShoot :: (Monad m, RandomGen g, ObjectCollection o) => Scene o -> ColorRGBf -> Int -> Vec3d -> Vec3d -> Vec3d -> Int -> RandT g m ([Photon], [Photon])
areaPhotonShoot sc pow cnt pos x2 y2 minBounce =
  let norm = normalize $ x2 |><| y2
   in do
        dirs <- repeatMF (toInteger cnt) $ cosWeightedDir norm
        xs <- (fmap (\a -> (2 * a -1) *| x2)) <$> getRandoms
        ys <- (fmap (\a -> (2 * a -1) *| y2)) <$> getRandoms
        (\(a, b) -> (concat a, concat b)) <$> unzip <$> (mapM (\((d, x, y) :: (Vec3d, Vec3d, Vec3d)) -> shootPhoton minBounce sc (EPhoton (Ray (pos |+| x |+| y) d) (pow |/ (fromIntegral cnt)))) $ take cnt $ zip3 dirs xs ys)

globalCam = Camera
  { cameraPos = V3 0 0 7,
    cameraDir = V3 0 0 (-1),
    cameraUp = V3 0 1 0,
    cameraFOV = 75,
    cameraAspect = 1
  }

launchWorker :: (Show a) => String -> Int -> [a] -> IO ProcessHandle
launchWorker bin wid xs =
  do
    writeFile ("job" ++ (show wid)) (show xs)
    spawnProcess bin ["worker", (show wid)]

joinWorker :: (Read a) => Int -> ProcessHandle -> IO [a]
joinWorker wid p =
  do
    _ <- waitForProcess p
    res <- read <$> (readFile $ "job" ++ (show wid))
    removeFile $ "job" ++ (show wid)
    return res

stripeList' n i v =
  if i >= VB.length v
    then []
    else (v VB.! i) : (stripeList' n (i + n) v)

stripeList n l =
  map (\i -> stripeList' n i v) [0 .. n -1]
  where
    v = VB.fromList l

unstripeLists :: [[a]] -> [a]
unstripeLists l =
  let vecs = VB.fromList $ map VB.fromList l
      totalLength = foldl (+) 0 $ fmap VB.length vecs
      n = VB.length vecs
      f i =
        if i >= totalLength
          then []
          else
            let vs =
                  if i + n > totalLength
                    then VB.slice 0 (totalLength - i) vecs
                    else vecs
                vals = VB.toList $ fmap (flip (VB.!) (i `div` n)) vs
             in (vals) ++ (f (i + n))
   in f 0

mpMap :: (Show a, Read b) => Int -> (Int -> [a] -> IO c) -> (Int -> c -> IO [b]) -> [a] -> IO [b]
mpMap n f_out f_in l =
  let groups = zip [0 ..] $ stripeList n l
   in do
        print $ "Group count: " ++ (show $ length groups)
        pids <- mapM (\(n, g) -> (f_out n g) >>= (\x -> return (n, x))) groups
        unstripeLists <$> (mapM (\(n, p) -> f_in n p) pids)

traceRays_ :: ObjectCollection o => RayTracer -> Scene o -> [(Integer, Ray)] -> IO [ColorRGBf]
traceRays_ rt sc = mapM (\x -> traceRay rt sc x)

runMaster :: [String] -> IO ()
runMaster (workerBin : (numProcsStr : (hstr : (outFile : _)))) =
  let incand = (V3 255 214 170)
      wht = constVec 255
      lightPow = ((50 / 255) *| wht)
      h = read hstr
      w = h
      rays = zip [1 ..] $ genRays globalCam h
      numProcs = read numProcsStr
   in do
        (sPhs, vPhs) <- evalRandIO $ areaPhotonShoot cornellBox lightPow 100000 (V3 0 1.998 0) (V3 0.5 0 0) (V3 0 0 0.5) 1 -- only indirect lighting
        pixs <-
          if numProcs == 1
            then workerMain sPhs vPhs rays
            else do
              writeFile "sPmap.bin" (show sPhs)
              writeFile "vPmap.bin" (show vPhs)
              mpMap numProcs (launchWorker workerBin) joinWorker rays
        saveToPngRtr outFile pixs w h

workerMain sPhs vPhs rays =
  let sPmap = mkPhotonMap sPhs
      vPmap = mkPhotonMap vPhs
      gil pmap pcount maxDist pos inc norm bssrdf = estimateRadiance pmap pcount pos inc maxDist bssrdf norm
      yesglil p = gil p 500 0.5
      noglil p _ _ _ _ = black
      volGlil pmap pCount maxDist pos inc phase = estimateVolumeRadiance pmap pCount pos inc maxDist phase
      yesVolGlil p = volGlil p 500 0.2
      rt = RayTracer
        { surfaceRadiance = yesglil sPmap,
          volumeRadiance = yesVolGlil vPmap
        }
   in traceRays_ rt cornellBox rays

runWorker :: [String] -> IO ()
runWorker (wid : _) =
  do
    sPhs <- read <$> (readFile "sPmap.bin")
    vPhs <- read <$> (readFile "vPmap.bin")
    rays <- (read <$> (readFile ("job" ++ wid))) :: IO [(Integer, Ray)]
    print $ take 10 $ map fst rays
    pixs <- workerMain sPhs vPhs rays
    print $ length pixs
    writeFile ("job" ++ wid) (show pixs)

printUsage :: IO ()
printUsage =
  do
    print "Usage: [master|worker] height"

main :: IO ()
main =
  let s = stripeList 10 [1 .. 999]
      b = unstripeLists s
   in -- pixs = pathTraceScene (globIllum pmap 200 1.0) (mkStdGen 0x1337beef) cornellBox cam h
      -- ptrcr = PathTracer {globalIllum = gil pmap 200 1.0}
      -- pixs = pathTraceScene ptrcr cornellBox h cam
      -- tph = shootPhoton cornellBox (mkStdGen 0xdeadbeef) (EPhoton (Ray (V3 0 0 0) (normalize $ V3 0.5 0.5 (-1))) whitef)
      -- genPmapImgs sPhs vPhs =
      --   do
      --     fbSurface <- return $ renderPhotons cornellBox cam h sPhs
      --     fbVolume <- return $ renderPhotons emptyScene cam h vPhs
      --     -- pixs <- return $ renderIlluminance (gil pmap 100 0.5) cornellBox cam h
      --     saveToPngPmap "surface_pmap.png" ((100 *|) <$> (fbPixelList fbSurface)) (fbWidth fbSurface) (fbHeight fbSurface)
      --     saveToPngPmap "volume_pmap.png" ((100 *|) <$> (fbPixelList fbVolume)) (fbWidth fbVolume) (fbHeight fbVolume)
      do
        args <- getArgs
        ops <-
          return
            [ ("master", runMaster $ tail args),
              ("worker", runWorker $ tail args)
            ]
        case lookup (args !! 0) ops of
          Nothing -> printUsage
          Just op -> op
-- TODO: list
-- Force stuff like luminance, radiance, etc. into type system to reduce mistakes; can we derive via?
--  TODO: Investigate inlining less aggressively and use more strictness to improve memory usage (each thread only needs memory for ONE rt op at a time, but its allocating memory for ALL RT ops ahead of time)
--  4. Use shoot photons in parallel (i.e. worker threads take chunks of work); I HAVE ACCESS TO PARALLEL.ecse still
--  ?. Support refraction by only accepting back-side hits (assume closed triangle meshes)
