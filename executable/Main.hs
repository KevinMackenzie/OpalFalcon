{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

-- import OpalFalcon.RayTracer

import Control.Monad.Random
import qualified Data.Array as A
import qualified Data.Vector as VB
import qualified Data.Vector as VB
import qualified Data.Vector.Storable as VS
import Debug.Trace
import OpalFalcon.BaseTypes
import OpalFalcon.Images
import qualified OpalFalcon.KdTree as Kd
import OpalFalcon.Material
import OpalFalcon.Math.Lighting
import qualified OpalFalcon.Math.MMesh as MM
import OpalFalcon.Math.Ray
import OpalFalcon.Math.Transformations
import qualified OpalFalcon.Math.TriMesh as TMesh
import OpalFalcon.Math.Vector
import OpalFalcon.Photon.Photon
import OpalFalcon.Photon.PhotonTracer
import OpalFalcon.Photon.Visualizer
import qualified OpalFalcon.Scene as Scene
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
-- import OpalFalcon.XTracing.PathTracer

import OpalFalcon.XTracing.RayTraceUtils
import OpalFalcon.XTracing.RayTracer
import OpalFalcon.XTracing.XTracer
import System.Directory
import System.Environment
import System.Process
import System.Random
import qualified TestScene as TS

sceneToRender :: Scene.Scene ObjectList
sceneToRender = TS.testScene0

printHits hs =
  foldl (++) "\n" $
    map
      ( \x -> case x of
          Nothing -> ""
          Just h -> show h
      )
      hs

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

traceRays_ :: Scene.ObjectCollection o => RayTracer -> Scene.Scene o -> [(Integer, Ray)] -> IO [ColorRGBf]
traceRays_ rt sc = mapM (\x -> traceRay rt sc x)

runMaster :: [String] -> IO ()
runMaster (workerBin : (numProcsStr : (hstr : (outFile : _)))) =
  let incand = (V3 255 214 170)
      lightPow = (50 *| whitef)
      h = read hstr
      w = h
      rays = zip [1 ..] $ genRays (Scene.sceneCamera sceneToRender) h
      numProcs = read numProcsStr
   in do
        -- (sPhs, vPhs) <- evalRandIO $ areaPhotonShoot cornellBox lightPow 100000 (V3 0 1.998 0) (V3 0.5 0 0) (V3 0 0 0.5) 1 -- only indirect lighting
        (sPhs, vPhs) <- evalRandIO $ shootPhotons 1 sceneToRender
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
   in traceRays_ rt sceneToRender rays

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
