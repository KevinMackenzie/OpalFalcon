{-# LANGUAGE RankNTypes #-}

{-# LANGAUGE ScopedTypeVariables #-}

module OpalFalcon.XTracing.RayTracer
  ( RayTracer (..),
    rayTraceScene,
    splitList,
    testParallel,
    testParallel2,
    traceRay,
  )
where

import Control.Concurrent
import qualified Control.Concurrent.Thread as Thread
import Control.Monad.Random
import Control.Parallel
import Data.Functor.Identity
import Data.Maybe
import Debug.Trace
import Debug.Trace
import GHC.Float (double2Float, float2Double)
import OpalFalcon.BaseTypes
import OpalFalcon.Math.Ray
import OpalFalcon.Math.Vector
import OpalFalcon.Scene
import OpalFalcon.Scene.Camera
import OpalFalcon.Util.Random
import OpalFalcon.XTracing.RayTraceUtils
import OpalFalcon.XTracing.XTracer
import System.Random
import System.Time.Extra (sleep)

numBounces :: Path -> Int
numBounces = length

-- TODO: We're assuming the global illumination does not include direct illumination
data RayTracer
  = RayTracer
      { surfaceRadiance :: SurfaceRadiance,
        volumeRadiance :: VolumeRadiance
      }

ptEpsilon = 0.00001

participateMedia :: (Monad m, RandomGen g, ObjectCollection o) => RayTracer -> Scene o -> ParticipatingMaterial -> Vec3d -> Vec3d -> ColorRGBf -> RandT g m ColorRGBf
-- lInPos is the position that the incoming radiance 'lInRad' enters the medium, and 'lOutPos' is the position that the incoming radiance, along with all in-scattered radiance, exits the medium (where the ray entered the medium)
-- 'probeInside' returns the intersection point of the medium
participateMedia rt sc (ParticipatingMaterial {participateAbsorb = absorb, participateScatter = scatter, participatePhase = phase, participateExit = probeInside}) lInPos lOutPos lInRad =
  -- Ray-march from the in position to the out position
  let extinct x = (absorb x) |+| (scatter x)
      albedo x = (scatter x) |/| (extinct x)
      advDist x xsi = (- (log xsi)) / (vecAvgComp $ extinct x) -- importance sampled distance between scattering events
      randNextEvt r@(Ray x _) = (\xsi -> (advanceRay r $ float2Double (advDist x xsi))) <$> getRandomNonZero
      lightDir = normalize $ lOutPos |-| lInPos
      attenuateMedium eventPos prevRad deltaPos = prevRad |*| (fmap (\x -> exp $ - x * (double2Float deltaPos)) (scatter eventPos))
      marchGeneric f stopPos prevPos prevRad =
        do
          -- TODO: This ray direction could be cached
          (Ray eventPos _) <- randNextEvt $ Ray prevPos $ normalize $ stopPos |-| prevPos
          if isBetween prevPos stopPos eventPos
            then-- The ray exited the volume before an event occured
              return prevRad
            else f eventPos prevPos prevRad >>= (marchGeneric f stopPos eventPos)
      -- factors in attenuation through the medium and in-scattering of light
      stepFull eventPos prevPos prevRad =
        let deltaPos = distance eventPos prevPos
            deltaPosF = double2Float deltaPos
            prevContrib = attenuateMedium eventPos prevRad deltaPos
            multiScattering = volumeRadiance rt eventPos lightDir phase
         in do
              -- Note: To avoid aliasing, jitter the sample location around 'eventPos'
              singleScattering <- directContribution eventPos
              return $ (singleScattering |* deltaPosF) |+| (multiScattering |* deltaPosF) |+| prevContrib
      -- Only factors in the attenuation through the medium
      stepDirectOnly samplePos prevPos prevRad =
        return $ attenuateMedium samplePos prevRad $ distance samplePos prevPos
      filterSample sPos (LightSample lPos _ _) =
        let fromLightDir = normalize $ sPos |-| lPos
         in case probeCollection (objects sc) (Ray lPos $ fromLightDir) of
              Nothing -> True
              Just h -> (hitPos h) ~= (probeInside (Ray sPos $ negateVec fromLightDir))
      directContribution samplePos =
        do
          samples <- (filter (filterSample samplePos)) <$> (sampleLights sc)
          contribs <-
            mapM
              ( \(LightSample lPos (AttenuationFunc atten) col) ->
                  marchGeneric
                    stepDirectOnly
                    samplePos
                    (probeInside $ Ray samplePos $ normalize $ lPos |-| samplePos)
                    (col |* (atten samplePos))
              )
              samples
          return $ vecSum contribs
   in marchGeneric stepFull lOutPos lInPos lInRad

-- DIrect illumination on surfaces
directIllum :: (Monad m, RandomGen g, ObjectCollection o) => Scene o -> Vec3d -> Vec3d -> Vec3d -> Bssrdf -> RandT g m ColorRGBf
directIllum scene pos rInDir norm (Bssrdf bssrdf) =
  let filterSample (LightSample lPos _ _) =
        case probeCollection (objects scene) (Ray lPos $ normalize $ pos |-| lPos) of
          Nothing -> True
          Just h -> (hitPos h) ~= pos
   in do
        -- TODO: Unify this somehow with the sampling code above
        samples <- (filter filterSample) <$> (sampleLights scene)
        contribs <-
          return $
            map -- This is not a mapM because the function isn't monadic
              ( \(LightSample lPos (AttenuationFunc atten) col) ->
                  (bssrdf (pos, normalize $ lPos |-| pos) (pos, rInDir)) |*| col |* (atten pos)
              )
              samples
        return $ vecSum contribs

traceRay :: (Monad m, RandomGen g, ObjectCollection o) => RayTracer -> Scene o -> (Integer, Ray) -> RandT g m ColorRGBf
traceRay rt sc ray =
  let glob = surfaceRadiance rt
      shootRay ray path
        | numBounces path > 5 = return $ V3 1 0 0 -- Too many bounces
        | otherwise = (shootRay' ray path)
      shootRay' ray@(Ray _ rDir) path =
        case probeCollection (objects sc) ray of
          Nothing -> return $ V3 0 1 0 -- Background color
          Just MkHit {hitPos = pos, hitNorm = norm, hitMat = m, hitInc = (Ray _ hDir)} ->
            let iDir = negateVec $ normalize hDir
                pass oPos oDir (Bssrdf bssrdf) = passRefl oPos oDir $ bssrdf (pos, iDir) (oPos, oDir)
                passRefl oPos oDir refl = (refl |*|) <$> (shootRay (advanceRay (Ray oPos oDir) ptEpsilon) (HSpec : path))
             in do
                  rayResult <- transmitRay m iDir
                  case rayResult of
                    RayTerm ->
                      liftM2
                        (|+|)
                        (return $ glob pos iDir norm (surfaceBssrdf m)) -- Indirect
                        (directIllum sc pos iDir norm (surfaceBssrdf m)) -- Direct
                    RayReflect oDir refl -> passRefl pos oDir refl
                    RayParticipate pMat ->
                      -- Increment hit position so we are sure we're inside the geometry
                      let lInPos = participateExit pMat $ advanceRay (Ray pos hDir) ptEpsilon
                          (Ray oPos oDir) = advanceRay (Ray lInPos hDir) ptEpsilon
                       in do
                            incomingRad <- pass oPos oDir (surfaceBssrdf m)
                            vecAverage <$> (replicateM 100 $ participateMedia rt sc pMat lInPos pos incomingRad)
      mf (n, x) = shootRay x []
   in mf ray

startThreads :: (a -> IO b) -> [a] -> IO [(ThreadId, IO (Thread.Result b))]
startThreads _ [] = return []
startThreads f (h : t) =
  do
    res <- Thread.forkOS $ do
      v <- f h
      return v
    tail <- startThreads f t
    return $ res : tail

joinThreads :: [(ThreadId, IO (Thread.Result a))] -> IO [a]
joinThreads [] = return []
joinThreads ((tid, resHandle) : t) =
  do
    res <- Thread.result =<< resHandle
    tail <- joinThreads t
    return $ res : tail

splitList :: Int -> [a] -> [[a]]
splitList n [] = []
splitList n l = (take n l) : (splitList n $ drop n l)

-- Eval each ray in the IO monad because the ray can't evaluate anything until the RNG is provided.  (This prevents large memory requirements)
mtMapM :: Int -> (a -> IO b) -> [a] -> IO [b]
mtMapM n f l =
  let n' = ((length l) `div` n) + 1
      groups = splitList n' l
   in do
        threads <-
          startThreads
            -- This forces evaluation of the whole list before the thread returns
            (\x -> (\vals -> (last vals) `seq` vals) <$> (mapM f x))
            groups
        concat <$> (joinThreads threads)

-- TODO: I'm not sure why this isn't parallelizing correct.  I suspect it has something to do with memory protection / mutexes since it doesn't happen with the test code at the bottom, but I do not know.
rayTraceScene :: (ObjectCollection o) => Int -> RayTracer -> Scene o -> Int -> Camera -> IO [ColorRGBf]
rayTraceScene n rt sc height cam =
  let l = zip [1 ..] (genRays cam height)
      f x = evalRandIO $ traceRay rt sc x
   in mtMapM n f l

testParallel2 n =
  do
    _ <- mtMapM n (\x -> (\y -> y * y) <$> (\y -> head $ (randoms y) :: Float) <$> getStdGen) [1 .. 10000000]
    return ()

testParallel =
  do
    (_, wait) <- Thread.forkIO $ do
      l <- (fmap (\x -> x * x)) <$> (\x -> take 100000000 $ ((randoms x) :: [Float])) <$> getStdGen
      print $ last l
      return ()
    l <- (fmap (\x -> x * x)) <$> (\x -> take 100000000 $ ((randoms x) :: [Float])) <$> getStdGen
    print $ last l
    x <- Thread.result =<< wait
    print $ show x
