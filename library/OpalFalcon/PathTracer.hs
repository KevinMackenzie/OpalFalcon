{-# LANGUAGE BangPatterns #-}
module OpalFalcon.PathTracer where

import GHC.Float
import Debug.Trace

import System.Random

import OpalFalcon.BaseTypes
import OpalFalcon.Scene
import OpalFalcon.RayTraceUtils
import OpalFalcon.Math.Ray
import OpalFalcon.Math.Vector

-- orElse :: a -> Maybe a -> a
-- orElse _ (Just x) = x
-- orElse x Nothing = x
-- 
-- data Rng g a = Rng g a
-- 
-- instance (RandomGen g) => Functor (Rng g) where
--     fmap f (Rng g a) = Rng g (f a)
-- 
-- instance Applicative Rng where
--     pure x = Rng mkStdGen x
--     (Rng _ a) <*> (Rng g b) = Rng g (a b)
-- 
-- instance (RandomGen g) => Monad (Rng g) where
--     (Rng g a) >>= f = Rng g (f a)

-- TODO: eventually, the probability of reflecting a diffuse surface will depend on how
--  diffuse it is.  Then consult global illumination.  Where do we draw the line between diffuse / specular.  Do we russian-roulette the eye-rays as well?
--  becuase photons are collected on mostly specular surfaces as well and maybe we some
--  material property that determines whether it should cosult global illumination or reflect, or maybe it does both all the time?
--
--  Maybe if we hit a high enough illuminence density on a hit, then we stop?  decaying exponentially with number of bounces

-- The values we get back from these functions are HDR, but still should conserve energy, so we need to convert back to color space displays can handle (i.e. logorithmic, square root, linear)

tracePath' :: RandomGen g => g -> (g -> Ray -> ColorRGBf) -> Vec3d -> Hit -> ColorRGBf
tracePath' g c iDir h =
    emit |+| ((c g' $ Ray (hitPos h) oDir') |*| refl |* angl)
    where m    = hitMat h
          (oDir, g') = tmap0 (clampHemisphere norm) $ random g
          oDir'= importance m iDir oDir
          refl = brdf m iDir oDir'
          emit = emittence m iDir -- This will be handled by photon mapping
          norm = hitNorm h
          angl = double2Float $ (negateVec iDir) |.| norm -- Projected solid angle

-- Since we do importance sampling, do we have to multi[ply by probability for sample -> i dont' think so...
-- Do we not factor in distence because this is a sampling method? -> i don't think so

-- If other functions are going to need the RNG, then it would make sense to wrap it in a ST monad
tracePath :: (RandomGen g, ObjectCollection o) => g -> Scene o -> Integer -> Integer -> Ray -> ColorRGBf
tracePath g s md d r@(Ray _ iDir)
    | d >= md   = V3 1 0 1
    | otherwise = case (probeCollection (objects s) r) of
                      Nothing -> V3 1 1 1
                      Just h  -> let recurse = (\g' -> tracePath g' s md (d+1))
                                 in  {-trace ("depth: " ++ (show d)) $-} tracePath' g recurse iDir h

randGens :: (RandomGen g) => g -> [g]
randGens g = g':gs
             where (g', g'') = split g
                   gs = randGens g''

pathTraceScene :: (RandomGen g, ObjectCollection o) => g -> Scene o -> Integer -> Integer -> Ray -> Double -> [ColorRGBf]
pathTraceScene g scene width height cameraRay fov = 
    map (\(g', r) -> tracePath g' scene 5 0 r) $
        zip (randGens g) $ genRays width height cameraRay fov

