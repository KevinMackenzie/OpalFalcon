module OpalFalcon.XTracing.PathTracer where

import Control.Monad.Random
import Debug.Trace
import OpalFalcon.BaseTypes
import OpalFalcon.Math.Ray (Ray (..))
import OpalFalcon.Math.Vector
import OpalFalcon.Scene
import OpalFalcon.Scene.Camera
import OpalFalcon.XTracing.RayTraceUtils
import OpalFalcon.XTracing.XTracer
import System.Random

-- Path representation
data PathTypes = PDiff | PSpec | PGloss

-- The ray path in reverse order (head is the previous bounce)
type Path = [PathTypes]

numBounces :: Path -> Int
numBounces = length

-- Maybe we can accumulate the BRDF weight as well, so our termination case is based on how much the next calculation would contribute to the sample.  This way we don't have to make concrete decisions based on limited path knowledge alone

data PathTracer = PathTracer {globalIllum :: GlobalIllum}

-- instance XTracer PathTracer where
--   renderScene = pathTraceScene

pathTraceScene :: (Monad m, RandomGen g, ObjectCollection o) => PathTracer -> Scene o -> Int -> Camera -> RandT g m ([ColorRGBf])
pathTraceScene pt sc height cam =
  let glob = globalIllum pt
      -- This will also determine things like whether to use approx or precise version of functions based on path
      shootRay ray path
        | numBounces path > 5 = return $ V3 1 0 0 -- Too many bounces
        | otherwise = shootRay' ray path
      -- Does the path-tracing using the photon map; may be other versions later
      shootRay' ray@(Ray _ rDir) path =
        case probeCollection (objects sc) ray of
          Nothing -> return $ V3 0 1 0 -- Background color
          Just MkHit {hitPos = pos, hitNorm = norm, hitMat = m, hitInc = (Ray _ hDir)} ->
            do
              coinFlip <- getRandom
              if coinFlip < (0.9 :: Float)
                then-- Note: This path-tracing solution only uses photon map for emittance
                --    but it could also consider single diffuse bounces for direct illumination
                  return $ glob pos rDir norm (brdf m) -- Emittance
                else do
                  -- Reflectance
                  randDir <- getRandom
                  let iDir = negateVec hDir
                      oDir = importance m iDir (clampHemisphere norm randDir)
                   in do
                        refl <- shootRay (Ray pos oDir) (PGloss : path)
                        return $ (brdf m iDir oDir) |*| refl
   in mapM (\(n, x) -> shootRay x []) $ zip [1 ..] $ genRays cam height
