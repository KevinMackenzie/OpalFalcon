{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OpalFalcon.XTracing.RayTracer
  ( RayTracer (..),
    rayTraceScene,
    splitList,
    testParallel,
    testParallel2,
    traceRay,
    mtMapM,
  )
where

import Control.Concurrent
import qualified Control.Concurrent.Thread as Thread
import Control.Monad.Random
import Control.Parallel
import Data.Functor.Identity
import Data.Maybe
import Debug.Trace
import GHC.Float (double2Float, float2Double)
import OpalFalcon.BaseTypes
import OpalFalcon.Math.Optics
import OpalFalcon.Math.Vector
import OpalFalcon.Scene
import OpalFalcon.Scene.Camera
import OpalFalcon.Util.Random
import qualified OpalFalcon.Util.StrictList as SL
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
participateMedia
  rt
  sc
  ( ParticipatingMaterial
      { participateAbsorb = absorb,
        participateScatter = scatter,
        participatePhase = phase,
        participateExit = probeInside
      }
    )
  lInPos
  lOutPos
  lInRad =
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
           in do
                -- Note: To avoid aliasing, jitter the sample location around 'eventPos'
                singleScattering <- directContribution eventPos
                multiScattering <- return $ volumeRadiance rt eventPos lightDir phase
                return $ (singleScattering |* deltaPosF) |+| (multiScattering |* deltaPosF) |+| prevContrib
        -- Only factors in the attenuation through the medium
        stepDirectOnly samplePos prevPos prevRad =
          return $ attenuateMedium samplePos prevRad $ distance samplePos prevPos
        filterSample sPos (LightSample lPos _ _) =
          let fromLightDir = normalize $ sPos |-| lPos
           in case probeCollection (sceneObjects sc) (Ray lPos $ fromLightDir) of
                Nothing -> True
                Just (_, h) -> case (probeInside (Ray sPos $ negateVec fromLightDir)) of
                  Nothing -> False -- Thin geometry
                  Just exHit -> (hitPos h) ~= (hitPos exHit)
        directContribution samplePos =
          do
            samples <- (filter (filterSample samplePos)) <$> (sampleLights sc)
            contribs <-
              mapM
                ( \(LightSample lPos (AttenuationFunc atten) col) ->
                    case (probeInside $ Ray samplePos $ normalize $ lPos |-| samplePos) of
                      Nothing -> return black -- Thin geometry
                      Just exHit ->
                        marchGeneric
                          stepDirectOnly
                          samplePos
                          (hitPos exHit)
                          (col |* (atten samplePos))
                )
                samples
            return $ vecSum contribs
     in marchGeneric stepFull lOutPos lInPos lInRad

subsurfaceScatter :: (Monad m, RandomGen g, ObjectCollection c) => RayTracer -> Scene c -> ParticipatingMaterial -> Vec3d -> Vec3d -> RandT g m ColorRGBf -> RandT g m (Vec3 Float)
subsurfaceScatter
  rt
  sc
  ( ParticipatingMaterial
      { participateAbsorb = absorb,
        participateScatter = scatter,
        participatePhase = phase,
        participateExit = probeInside
      }
    )
  lInPos
  lOutPos
  lInRad =
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
        marchBackwards f stopPos prevPos depth =
          -- Technically this is the one that's forwards
          let progress =
                do
                  -- TODO: This ray direction could be cached
                  (Ray eventPos _) <- randNextEvt $ Ray prevPos $ normalize $ stopPos |-| prevPos
                  if isBetween prevPos stopPos eventPos
                    then-- The ray exited the volume before an event occured
                      lInRad
                    else
                      marchBackwards
                        f
                        stopPos
                        eventPos
                        ((double2Float $ distance prevPos eventPos) * (vecAvgComp $ extinct eventPos) + depth)
                        >>= (f eventPos prevPos)
           in if depth > 2-- TODO: how to russian roulette this?
                then do
                  xsi <- getRandom
                  if xsi > (0.5 :: Float)
                    then return black
                    else progress
                else progress
        -- factors in attenuation through the medium and in-scattering of light
        stepFull eventPos prevPos prevRad =
          let deltaPos = distance eventPos prevPos
              deltaPosF = double2Float deltaPos
              prevContrib = attenuateMedium eventPos prevRad deltaPos
           in do
                -- Note: To avoid aliasing, jitter the sample location around 'eventPos'
                singleScattering <- directContribution eventPos
                multiScattering <- return $ volumeRadiance rt eventPos lightDir phase
                return $ (singleScattering |* deltaPosF) |+| (multiScattering |* deltaPosF) |+| prevContrib
        -- Only factors in the attenuation through the medium
        stepDirectOnly samplePos prevPos prevRad =
          return $ attenuateMedium samplePos prevRad $ distance samplePos prevPos
        filterSample sPos (LightSample lPos _ _) =
          let fromLightDir = normalize $ sPos |-| lPos
           in case probeCollection (sceneObjects sc) (Ray lPos $ fromLightDir) of
                Nothing -> True
                Just (_, h) -> case (probeInside (Ray sPos $ negateVec fromLightDir)) of
                  Nothing -> False -- Thin geometry
                  Just exHit -> (hitPos h) ~= (hitPos exHit)
        directContribution samplePos =
          do
            samples <- (filter (filterSample samplePos)) <$> (sampleLights sc)
            contribs <-
              mapM
                ( \(LightSample lPos (AttenuationFunc atten) col) ->
                    case (probeInside $ Ray samplePos $ normalize $ lPos |-| samplePos) of
                      Nothing -> return black -- Thin geometry
                      Just exHit ->
                        marchGeneric
                          stepDirectOnly
                          samplePos
                          (hitPos exHit)
                          (col |* (atten samplePos))
                )
                samples
            return $ vecSum contribs
     in marchBackwards stepFull lInPos lOutPos 0

-- DIrect illumination on surfaces
directIllum :: (Monad m, RandomGen g, ObjectCollection o) => Scene o -> Vec3d -> Vec3d -> Vec3d -> Bssrdf -> RandT g m ColorRGBf
directIllum scene pos rInDir norm (Bssrdf bssrdf) =
  let filterSample (LightSample lPos _ _) =
        case probeCollection (sceneObjects scene) (Ray lPos $ normalize $ pos |-| lPos) of
          Nothing -> True
          Just (_, h) -> (hitPos h) ~= pos
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

traceRay :: (ObjectCollection o) => RayTracer -> Scene o -> (Integer, Ray) -> IO ColorRGBf
traceRay rt sc ray =
  let glob = surfaceRadiance rt
      shootRay ray path
        | numBounces path > 5 = return $ V3 1 0 0 -- Too many bounces
        | otherwise = (shootRay' ray path)
      shootRay' ray path =
        case probeCollection (sceneObjects sc) ray of
          Nothing -> return $ V3 0 1 0 -- Background color
          Just (obj, h@(MkHit {hitPos = pos, hitNorm = norm, hitInc = (Ray _ hDir)})) ->
            let iDir = negateVec $ normalize hDir
                pass oRay = shootRay (advanceRay oRay ptEpsilon) (HSpec : path)
                passRefl oRay refl = (refl |*|) <$> (pass oRay)
                m = objHitMat obj h
             in do
                  rayResult <- transmitRay m iDir
                  case rayResult of
                    RayTerm ->
                      let indirect = glob pos iDir norm (surfaceBssrdf m)
                       in do
                            direct <- directIllum sc pos iDir norm (surfaceBssrdf m)
                            return $ indirect |+| direct
                    RayReflect oDir refl -> passRefl (Ray pos oDir) refl
                    RayParticipate pMat ->
                      -- Increment hit position so we are sure we're inside the geometry
                      case participateExit pMat $ advanceRay (Ray pos hDir) ptEpsilon of
                        Nothing -> pass $ Ray pos hDir -- Thin geometry
                        Just lInHit ->
                          do
                            incomingRad <- pass $ advanceRay (Ray (hitPos lInHit) hDir) ptEpsilon
                            samples <- replicateM 100 (participateMedia rt sc pMat (hitPos lInHit) pos incomingRad)
                            return $ vecAverage samples
                    RayScatter ScatteringMaterial {scatteringParticipate = pMat, scatteringRefract = ior} ->
                      let (Just newRay) = refractIRay (IRay $ Ray pos hDir) (norm3 norm) 1.0 ior
                       in case participateExit pMat $ advanceRay newRay ptEpsilon of
                            Nothing -> pass $ Ray pos hDir -- Thin geometry
                            Just lInHit ->
                              let eRayM =
                                    refractIRay
                                      (IRay $ Ray (hitPos lInHit) (rayDir $ hitInc lInHit))
                                      (norm3 $ negateVec $ hitNorm lInHit)
                                      ior
                                      1.0
                               in case eRayM of
                                    Nothing -> return $ V3 0 0 1
                                    Just (exitingRay) -> do
                                      vecAverage
                                        <$> ( replicateM
                                                100
                                                ( subsurfaceScatter
                                                    rt
                                                    sc
                                                    pMat
                                                    (hitPos lInHit)
                                                    pos
                                                    (pass $ advanceRay exitingRay ptEpsilon)
                                                )
                                            )
      mf (n, x) = evalRandIO $ shootRay x []
   in mf ray

startThreads :: (a -> IO b) -> [a] -> IO [(ThreadId, IO (Thread.Result b))]
startThreads _ [] = return []
startThreads f (h : t) =
  do
    res <- Thread.forkOS $ f h
    tail <- startThreads f t
    return $ res : tail

joinThreads :: [(ThreadId, IO (Thread.Result a))] -> IO [a]
joinThreads [] = return []
joinThreads ((tid, resHandle) : t) =
  do
    res <- Thread.result =<< resHandle
    tail <- joinThreads t
    return $ res : tail

splitList :: Int -> [a] -> [SL.List a]
splitList n [] = []
splitList n l = (SL.fromList $ take n l) : (splitList n $ drop n l)

-- Eval each ray in the IO monad because the ray can't evaluate anything until the RNG is provided.  (This prevents large memory requirements)
mtMapM :: Int -> (a -> IO b) -> [a] -> IO [b]
mtMapM n f l =
  let n' = ((length l) `div` n) + 1
      groups = splitList n' l
   in do
        threads <-
          startThreads (mapM f) groups
        concat <$> (fmap SL.toList) <$> (joinThreads threads)

-- TODO: I'm not sure why this isn't parallelizing correct.  I suspect it has something to do with memory protection / mutexes since it doesn't happen with the test code at the bottom, but I do not know.
rayTraceScene :: (ObjectCollection o) => Int -> RayTracer -> Scene o -> Int -> IO [ColorRGBf]
rayTraceScene n rt sc height =
  let l = zip [1 ..] (genRays (sceneCamera sc) height)
      f x = traceRay rt sc x
   in mtMapM n f l

testParallel2 n =
  do
    res <-
      mtMapM
        n
        (\_ -> (foldl (+) 2) <$> (\y -> take 100 $ (randoms y) :: [Float]) <$> getStdGen)
        [1 .. 1000000]
    print $ last res
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
