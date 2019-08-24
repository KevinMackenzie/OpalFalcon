module Main where

import OpalFalcon.BaseTypes
import OpalFalcon.Math.Vector
import OpalFalcon.Math.Transformations
import OpalFalcon.Math.Ray
import OpalFalcon.RayTracer
import OpalFalcon.Images
import OpalFalcon.Scene.ObjectCollections.ObjectList
import OpalFalcon.Scene.Objects
import OpalFalcon.Scene
import OpalFalcon.Material
import OpalFalcon.Scene.Objects.Sphere
import OpalFalcon.Scene.Objects.Plane
import OpalFalcon.Scene.Objects.Disc
import OpalFalcon.Scene.Objects.PointLight

-- This is a test to see if we can get a simple rendering of a sphere
-- TODO: we need a way to save the results to a file (do ppm for now)

sph :: Object
sph1 :: Object
ground :: Object
ol :: ObjectList
sc :: Scene ObjectList

sph = mkSphereObject (MkSphere (mkAffineSpace (V3 (-2) 1.3 (-2)) xAxis yAxis zAxis) 1.5) (\_ _ -> mkSimpleMat (V3 0 1 0) (V3 0.8 0.4 0.4))
sph1 = mkSphereObject (MkSphere (mkAffineSpace (V3 2 1.3 (-2)) xAxis yAxis zAxis) 1.5) (\_ _ -> mkSimpleMat (V3 0 1 0) (V3 0.4 0.8 0.4))
ground = mkPlaneObject (MkPlane (mkAffineSpace (V3 0 (-2) 0) xAxis (negateVec zAxis) yAxis)) (\_ _ -> mkDiffuseMat (V3 0.5 0.5 0.5))
dPlane = MkPlane (mkAffineSpace (V3 (-2) 4 (-2)) xAxis zAxis (negateVec yAxis))
disc = mkDiscObject (MkDisc dPlane 2) (\_ _ -> mkDiffuseMat (V3 0.5 0.8 0.4))
ol = MkObjList { objList = [sph, sph1, ground, disc] }
eol = MkObjList { objList = [disc] }
sc = (MkScene {objects=ol, lightSources = [
        -- mkPointLight (V3 (-4) 0 0) (V3 1 1 1) 10,
        -- mkPointLight (V3 6 0 2) (V3 1 1 1) 10,
        mkDiscLight (MkDisc dPlane 2) whitef 100
    ]})

printHits hs = foldl (++) "\n" $ map (\x -> case x of Nothing -> ""
                                                      Just h -> show h) hs

main :: IO ()
main = let w = 500
           h = 500
           pixs = rayTraceScene sc w h (MkRay (V3 0 1 5) (normalize $ V3 0 (-0.2) (-1))) 90.0
           -- grads = (map (\(x,y) -> V3 (fromInteger x) (fromInteger y) 0.0) (genPixMap 25 25))
           -- hits = debugHits 0 sc 25 25 (V3 0 0 10) (V3 0 0 (-1)) 75.0
           -- r0 = defaultRtRay $ MkRay (V3 0 0 10) (V3 0 0 (-1))
           -- h0 = probeCollection ol $ rayBase r0
           -- r1 = fmap (deriveRay r0) h0
           -- h1 = fmap ((probeCollection ol) . rayBase) r1
           -- hitPlane = hittestPlane (MkPlane (MkRay (V3 0 (-3) 0) (V3 0 1 0))) (MkRay origin $ normalize (V3 0 (-1) (-1)))
           ls = sampleLights (gray 0.1) sc $ V3 0 (-2) 0
       in  do {
           -- writeFile "outfile.ppm" $ encodePpm pixs 100 100;
           saveToPng "pngfile.png" pixs w h;
           -- writeFile "hittests.txt" $ printHits hits
           -- print $ reflect (V3 0 0 1) (normalize (V3 0 1 1));
           -- print $ reflectRay (MkRay (V3 0 0 1) (V3 0 0 (-1))) (normalize (V3 1 1 1));
           -- print $ closerPos (V3 0 0 0) (Just (V3 1 2 1)) (Just (V3 2 1 2));
           -- print $ hittestSphere (MkSphere (V3 0 0 (-1)) 2) (MkRay (V3 0 0 10) (V3 0 0 (-1)));
           -- print $ objIntersectRay sph (MkRay (V3 0 0 10) (V3 0 0 (-1)));
           print $ ls;
           -- print $ hitPlane;
           }

-- TODO: uhh, write some fucking tests.  That's how you debug FP

