{-# LANGUAGE RankNTypes #-}

module OpalFalcon.XTracing.RayTracer
  ( RayTracer (..),
    rayTraceScene,
    splitList,
  )
where

import Control.Concurrent
import qualified Control.Concurrent.Thread as Thread
import Control.Monad.Random
import Control.Parallel
import Data.Functor.Identity
import Debug.Trace
import Debug.Trace
import OpalFalcon.BaseTypes
import OpalFalcon.Math.Ray (Ray (..))
import OpalFalcon.Math.Vector
import OpalFalcon.Scene
import OpalFalcon.Scene.Camera
import OpalFalcon.Util.Random
import OpalFalcon.XTracing.RayTraceUtils
import OpalFalcon.XTracing.XTracer
import System.Random

numBounces :: Path -> Int
numBounces = length

-- TODO: We're assuming the global illumination does not include direct illumination
data RayTracer = RayTracer {globalIllum :: GlobalIllum}

ptEpsilon = 0.00001

directIllum scene pos idir norm brdf = sampleLights scene (mkRayBrdf brdf idir) pos

traceRays :: (Monad m, RandomGen g, ObjectCollection o) => RayTracer -> Scene o -> [Ray] -> RandT g m [ColorRGBf]
traceRays pt sc rays =
  let glob = globalIllum pt
      shootRay ray path
        | numBounces path > 5 = return $ V3 1 0 0 -- Too many bounces
        | otherwise = (shootRay' ray path)
      shootRay' ray@(Ray _ rDir) path =
        case probeCollection (objects sc) ray of
          Nothing -> return $ V3 0 1 0 -- Background color
          Just MkHit {hitPos = pos, hitNorm = norm, hitMat = m, hitInc = (Ray _ hDir)} ->
            let iDir = negateVec $ normalize hDir
             in do
                  rayResult <- transmitRay m iDir black
                  case rayResult of
                    RayTerm ->
                      liftM2
                        (|+|)
                        (return $ glob pos iDir norm (photonBrdf m)) -- Indirect
                        (directIllum sc pos iDir norm (photonBrdf m)) -- Direct
                    RayPass oDir refl -> (refl |*|) <$> (shootRay (Ray (pos |+| (ptEpsilon *| oDir)) oDir) (HSpec : path))
      mf (n,x) = shootRay x []
   in mapM mf $ zip [1 ..] rays

-- TODO: This _is_ parallel, but for some reason its not any faster
startThreads :: (a -> IO b) -> [a] -> IO [(ThreadId, IO (Thread.Result b))]
startThreads _ [] = return []
startThreads f (h : t) =
  do
    res <- Thread.forkIO (f h)
    tail <- startThreads f t
    return $ res : tail

joinThreads :: [(ThreadId, IO (Thread.Result a))] -> IO [a]
joinThreads [] = return []
joinThreads ((tid, resHandle) : t) =
  do
    curr <- resHandle
    res <- Thread.result curr
    tail <- joinThreads t
    return $ res : tail

rayTraceScene :: (ObjectCollection o) => Int -> RayTracer -> Scene o -> Int -> Camera -> IO [ColorRGBf]
rayTraceScene n rt sc height cam =
  let l = genRays cam height
      n' = ((length l) `div` n) + 1
      groups = splitList n' l
   in do
        threads <- startThreads (\x -> evalRandIO $ traceRays rt sc x) groups
        fmap concat $ joinThreads threads

splitList :: Int -> [a] -> [[a]]
splitList n [] = []
splitList n l = (take n l) : (splitList n $ drop n l)

mapPar f (h : t) = v `par` (vt `pseq` (v : vt))
  where
    v = f h
    vt = mapPar f t
mapPar _ [] = []

mapMParallel :: RandomGen g => g -> Int -> (a -> RandT g Identity b) -> [a] -> [b]
mapMParallel gen n f l =
  let n' = ((length l) `div` n) + 1
      groups = splitList n' l
      ps = zip groups $ randGens gen
      res = mapPar (\(x, y) -> evalRand (mapM f x) y) ps
   in concat res
