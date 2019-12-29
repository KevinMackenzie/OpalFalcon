{-# LANGUAGE BangPatterns #-}
module Main where

import System.Random
import qualified Data.Array as A
import OpalFalcon.BaseTypes
import OpalFalcon.Math.Vector
import OpalFalcon.Math.Transformations
import OpalFalcon.Math.Ray
import OpalFalcon.PathTracer
-- import OpalFalcon.RayTracer
import OpalFalcon.Images
import OpalFalcon.Scene.ObjectCollections.ObjectList
import OpalFalcon.Scene.Objects
import OpalFalcon.Scene
import OpalFalcon.Material
import OpalFalcon.Scene.Objects.Sphere
import OpalFalcon.Scene.Objects.Plane
import OpalFalcon.Scene.Objects.Disc
import OpalFalcon.Scene.Objects.PointLight
import OpalFalcon.Photon.Photon
import OpalFalcon.Photon.PhotonTracer
import OpalFalcon.KdTree
import OpalFalcon.Scene.Camera
import OpalFalcon.Photon.Visualizer

-- This is a test to see if we can get a simple rendering of a sphere
-- TODO: we need a way to save the results to a file (do ppm for now)

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
ol = MkObjList { objList = [sph, sph1, ground, disc] }
eol = MkObjList { objList = [disc] }
sc = (MkScene {objects=ol, lightSources = [
        -- mkPointLight (V3 (-4) 0 0) (V3 1 1 1) 10,
        -- mkPointLight (V3 6 0 2) (V3 1 1 1) 10,
        mkDiscLight (MkDisc dPlane 2) whitef 100
    ]})

printHits hs = foldl (++) "\n" $ map (\x -> case x of Nothing -> ""
                                                      Just h -> show h) hs

-- Generate a bunch of points on a sphere
spherePoints lonSteps latSteps radius pos = 
    [
        (V3 (radius * (cos theta) * (cos phi)) 
            (radius * (sin theta)) 
            (radius * (cos theta) * (sin phi))) |+| pos
        | theta <- [(n*pi/latSteps - pi/2) | n <- [1..(latSteps-1)]], 
          phi <- [(n*2*pi / lonSteps) | n <- [1..(lonSteps-1)]]
    ]

-- Filters the Nothing values out of a list
filterJust :: [Maybe a] -> [a]
filterJust [] = []
filterJust ((Nothing):xs) = filterJust xs
filterJust ((Just x):xs) = x:(filterJust xs)

spherePhotonShoot sc pow lonSteps latSteps pos = filterJust $ map (\(d,r) -> shootPhoton sc r (EPhoton (Ray pos d) pow)) $ zip (spherePoints lonSteps latSteps 1 origin) $ randGens $ mkStdGen 0x1337dead

main :: IO ()
main = let w = 500
           h = 500
           -- pixs = pathTraceScene (mkStdGen 0x1337dead) sc w h (Ray (V3 0 1 5) (normalize $ V3 0 (-0.2) (-1))) 90.0
           -- px = tracePath (mkStdGen 0x1337dead) sc 2 0 (Ray (V3 0 1 5) (normalize $ V3 (-0.1) 0 (-1)))
           -- (Just ph) = shootPhoton sc (mkStdGen 0x1337dead) (EPhoton (Ray (V3 0 1 5) (normalize $ V3 (-0.1) 0 (-1))) (V3 10.0 0.0 0.0))
           phs = spherePhotonShoot sc whitef 3000 1000 (V3 0 (-1) (-5))
           cam = Camera { cameraPos = V3 0 1 5, cameraDir = normalize $ V3 0 (-0.2) (-1), cameraUp = V3 0 1 0, cameraFOV = 90, cameraAspect = 1 }
           fb = renderPhotons sc cam h phs
           -- sphPts = spherePoints 31 10 3 (V3 0 0 2)
           -- fakePhs = map (\x -> Photon x whitef origin XAxis) sphPts
           -- fakeFb = renderPhotons sc cam h fakePhs
       in  do {
           -- saveToPngRtr "pngfile.png" pixs w h;
           saveToPngPmap "pmap.png" (fbPixelList fb) (fbWidth fb) (fbHeight fb);
           -- saveToPngPmap "pmap.png" (fbPixelList fakeFb) (fbWidth fakeFb) (fbHeight fakeFb);
           -- putStr $ foldl (\x y -> x ++ "\n" ++ (show y)) "" sphPts;
           -- print px;
           }
