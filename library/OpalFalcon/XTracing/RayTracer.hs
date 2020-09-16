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
            prevContrib = attenuateMedium eventPos prevRad deltaPos
            multiScattering = volumeRadiance rt eventPos lightDir phase
         in do
              -- Note: To avoid aliasing, jitter the sample location around 'eventPos'
              singleScattering <- directContribution eventPos
              return $ prevContrib |+| multiScattering |+| singleScattering
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

traceRays :: (Monad m, RandomGen g, ObjectCollection o) => RayTracer -> Scene o -> [Ray] -> RandT g m [ColorRGBf]
traceRays rt sc rays =
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
                            participateMedia rt sc pMat lInPos pos incomingRad
      mf (n, x) = id $! shootRay x []
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
    res <- Thread.result =<< resHandle
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
