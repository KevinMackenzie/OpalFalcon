{-# LANGUAGE FlexibleInstances #-}

-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.

-- Hspec is one of the providers for Tasty. It provides a nice syntax for
-- writing tests. Its website has more info: <https://hspec.github.io>.

import Control.DeepSeq
import Control.Exception as E
import Control.Monad
import Data.CallStack
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.STRef
import qualified Data.Set as Set
import qualified Data.Vector as VB
import qualified Data.Vector.Storable as VS
import GHC.ST (ST (..), runST)
import OpalFalcon.BaseTypes
import qualified OpalFalcon.KdTree as KD
import OpalFalcon.Math
import qualified OpalFalcon.Math.ConvexHull as CH
import qualified OpalFalcon.Math.MMesh as MM
import qualified OpalFalcon.Math.Matrix as M
import qualified OpalFalcon.Math.Optics as Optics
import OpalFalcon.Math.Optics
import OpalFalcon.Math.Transformations
import qualified OpalFalcon.Math.TriMesh as TMesh
import OpalFalcon.Math.Vector
import qualified OpalFalcon.Photon.Photon as PH
import qualified OpalFalcon.Photon.STHeap as STH
import OpalFalcon.Scene.Camera
import qualified OpalFalcon.Scene.Objects.Triangle as OTRI
import qualified OpalFalcon.Util.Misc as Misc
import qualified OpalFalcon.Util.MutableList as MList
import System.Random
import Test.HUnit.Lang
import qualified Test.Tasty
import Test.Tasty.Hspec

location :: HasCallStack => Maybe SrcLoc
location = case reverse callStack of
  (_, loc) : _ -> Just loc
  [] -> Nothing

-- Like `assertEqual`, but it works with user-provided predicates
assertUser predicate preface actual expected =
  unless (predicate actual expected) $ do
    (prefaceMsg `deepseq` expectedMsg `deepseq` actualMsg `deepseq` E.throwIO (HUnitFailure location $ ExpectedButGot prefaceMsg expectedMsg actualMsg))
  where
    prefaceMsg
      | null preface = Nothing
      | otherwise = Just preface
    expectedMsg = show expected
    actualMsg = show actual

assertApproxEqual :: (Show (a b), Vector a, Floating b, Ord b) => b -> a b -> a b -> IO ()
assertApproxEqual e = assertUser (approxEq e) ""

shouldBeApprox :: (Show (a b), Vector a, Floating b, Ord b) => a b -> a b -> IO ()
shouldBeApprox = assertApproxEqual 0.001

assertApproxEqualS :: (Show a, Floating a, Ord a) => a -> a -> a -> IO ()
assertApproxEqualS e = assertUser (\a b -> abs (a - b) < e) ""

shouldBeApproxS :: (Show a, Floating a, Ord a) => a -> a -> IO ()
shouldBeApproxS = assertApproxEqualS 0.001

main :: IO ()
main = do
  kdTreeTests <- testSpec "OpalFalcon.KdTree" kdTreeSpec
  photonTests <- testSpec "OpalFalcon.Photon.Photon" photonSpec
  convexHullTests <- testSpec "OpalFalcon.Math.ConvexHull" convexHullSpec
  vectorTests <- testSpec "OpalFalcon.Math.Vector" vectorSpec
  matrixTests <- testSpec "OpalFalcon.Math.Matrix" matrixSpec
  transformationsTests <- testSpec "OpalFalcon.Math.Transformations" transformationsSpec
  cameraTests <- testSpec "OpalFalcon.Scene.Camera" cameraSpec
  triangleTests <- testSpec "OpalFalcon.Scene.Objects.Triangle" triangleSpec
  stHeapTests <- testSpec "OpalFalcon.Photon.STHeap" stHeapSpec
  triMeshTests <- testSpec "OpalFalcon.Math.TriMesh" triMeshSpec
  utilTests <- testSpec "OpalFalcon.Util" utilSpec
  opticsTests <- testSpec "OpalFalcon.Math" opticsSpec
  Test.Tasty.defaultMain $
    Test.Tasty.testGroup
      "OpalFalcon"
      [ kdTreeTests,
        photonTests,
        convexHullTests,
        vectorTests,
        matrixTests,
        transformationsTests,
        cameraTests,
        triangleTests,
        stHeapTests,
        triMeshTests,
        utilTests,
        opticsTests
      ]

-- Only test non-trivial operations on vectors
vectorSpec :: Spec
vectorSpec = parallel $ do
  describe "Vector" $ do
    describe "approximately equal" $ do
      it "works" $ do
        ((V3 0 0 (0.3333333333 :: Double)) ~= (V3 0 0 (1 / 3))) `shouldBe` True
    describe "cross product" $ do
      it "has correct handed-ness" $ do
        ((V3 1 0 0) |><| (V3 0 1 0)) `shouldBe` (V3 0 0 1)
      it "is zero when parallel" $ do
        ((V3 0 0 1) |><| (V3 0 0 1)) `shouldBe` (V3 0 0 0)
      it "is zero when antiparallel" $ do
        ((V3 0 0 1) |><| (V3 0 0 (-1))) `shouldBe` (V3 0 0 0)
    describe "reflect over normal" $ do
      it "reflects normal" $ do
        (reflect (V3 0 0 1) (V3 0 0 (-1))) `shouldBe` (V3 0 0 1)
      it "reflects 3d" $ do
        ((reflect (V3 0 0 1) (normalize (V3 1 1 1))) `shouldBeApprox` (V3 (2 / 3) (2 / 3) (-1 / 3)))

-- Test all matrix operations and their properties
matrixSpec :: Spec
matrixSpec = parallel $ do
  describe "Matrix" $ do
    it "multiplies" $ do
      (V3 (V3 1 4 7) (V3 2 5 8) (V3 3 6 9)) M.||*|| (V3 (V3 10 13 16) (V3 11 14 17) (V3 12 15 18)) `shouldBe` (V3 (V3 84 201 318) (V3 90 216 342) (V3 96 231 366))
    it "does not commute" $
      let x = (V3 (V3 1 2 3) (V3 4 5 6) (V3 7 8 9))
          y = (V3 (V3 2 3 4) (V3 3 4 5) (V3 4 5 6))
       in do
            (x M.||*|| y) `shouldNotBe` (y M.||*|| x)
    it "transposes" $ do
      (M.transpose (V4 (V4 1 2 3 4) (V4 5 6 7 8) (V4 9 10 11 12) (V4 13 14 15 16))) `shouldBe` (V4 (V4 1 5 9 13) (V4 2 6 10 14) (V4 3 7 11 15) (V4 4 8 12 16))

-- Tests transformation semantics and non-trivial transformations
transformationsSpec :: Spec
transformationsSpec = parallel $ do
  describe "Transformations" $ do
    it "apply correctly" $ do
      (applyTransform (scale (V3 1 2 3)) (V4 1 2 3 4)) `shouldBe` (V4 1 4 9 4)
      (applyTransform3 M.identity (V3 1 2 3)) `shouldBe` (V3 1 2 3)
      (applyTransformStack [M.identity] (V4 1 2 3 4)) `shouldBe` (V4 1 2 3 4)
    it "apply in the correct order" $
      let x = (V4 (V4 1 2 3 4) (V4 5 6 7 8) (V4 9 10 11 12) (V4 13 14 15 16))
          y = (V4 (V4 11 12 13 14) (V4 15 16 17 18) (V4 19 110 111 112) (V4 113 114 115 116))
          z = x M.||*|| y
          v = V4 1 2 3 4
          vt = applyTransform z v
          vt1 = foldr (applyTransform) v [x, y]
          vt2 = applyTransformStack [x, y] v
       in do
            (vt1 `shouldBe` vt)
            (vt2 `shouldBe` vt)
    it "looks at correctly" $ do
      ((lookAt (V3 0 0 1)) `shouldBe` M.identity)

cameraSpec :: Spec
cameraSpec = parallel $ do
  describe "Camera" $ do
    describe "Trasformations" $ do
      it "rotate" $
        let cam0 = Camera
              { cameraPos = origin,
                cameraDir = (V3 0 0 (-1)),
                cameraUp = V3 0 1 0,
                cameraFOV = 90.0,
                cameraAspect = 1
              }
            cam1 = Camera
              { cameraPos = origin,
                cameraDir = normalize (V3 0 (-1) (-1)),
                cameraUp = V3 0 1 0,
                cameraFOV = 90.0,
                cameraAspect = 1
              }
            obj0 = V3 0 0 (-1)
         in do
              ((applyTransform3 (cameraViewTransform cam0) obj0) `shouldBe` obj0)
              ((applyTransform3 (cameraViewTransform cam1) obj0) `shouldBeApprox` (V3 0 0.5 (- (sqrt 2) / 2)))
      it "translate" $
        let cam0 = Camera
              { cameraPos = V3 0 0 1,
                cameraDir = (V3 0 0 (-1)),
                cameraUp = V3 0 1 0,
                cameraFOV = 90.0,
                cameraAspect = 1
              }
            obj0 = V3 0 0 (-1)
         in do
              ((applyTransform3 (cameraViewTransform cam0) obj0) `shouldBe` (V3 0 0 (-2)))
      it "perspective projection" $
        let cam0 = Camera
              { cameraPos = V3 0 0 1,
                cameraDir = (V3 0 0 (-1)),
                cameraUp = V3 0 1 0,
                cameraFOV = deg2Rad 90,
                cameraAspect = 1
              }
            -- Flatten to 2d space since we don't need depth info
            tf = applyTransformStack3 [orthoZ, cameraProjTransform cam0]
            obj0 = V3 0 0 (-1)
            obj1 = V3 0 (-10) (-10)
         in do
              ((tf obj0) `shouldBeApprox` (V3 0 0 0))
              ((tf obj1) `shouldBeApprox` (V3 0 (-1) 0))
      it "image/pixel-space" $
        let cam0 = Camera
              { cameraPos = V3 0 0 1,
                cameraDir = (V3 0 0 (-1)),
                cameraUp = V3 0 1 0,
                cameraFOV = deg2Rad 90,
                cameraAspect = 16 / 9
              }
            tf = applyTransform3 (cameraPixelTransform cam0 1080)
            obj0 = V3 0 0 0
            obj1 = V3 (-0.25) (-0.25) 0
         in do
              ((tf obj0) `shouldBe` (V3 (1920 / 2) (1080 / 2) 0))
              ((tf obj1) `shouldBe` (V3 690 270 0))

triangleSpec :: Spec
triangleSpec =
  describe "Triangle" $ do
    describe "Intersection" $
      let t0 = OTRI.MkTriangle (V3 (-1) (-1) (-2)) (V3 1 0 (-2)) (V3 0 1 (-2))
       in do
            it "hits front-face (from +z)" $
              let r = Ray (V3 0 0 1) (V3 0 0 (-1))
                  mh = OTRI.hittestTriangle t0 r
                  hit = fromJust mh
               in do
                    ((isJust mh) `shouldBe` True)
                    ((hitParam hit) `shouldBe` 3)
                    ((hitPos hit) `shouldBe` (V3 0 0 (-2)))
                    ((hitNorm hit) `shouldBe` zAxis)
                    ((hitInc hit) `shouldBe` r)
            it "hits front-face (from -z)" $
              let r = Ray (V3 0 0 (-1)) (V3 0 0 (-1))
                  mh = OTRI.hittestTriangle t0 r
                  hit = fromJust mh
               in do
                    ((isJust mh) `shouldBe` True)
                    ((hitParam hit) `shouldBe` 1)
                    ((hitPos hit) `shouldBe` (V3 0 0 (-2)))
                    ((hitNorm hit) `shouldBe` zAxis)
                    ((hitInc hit) `shouldBe` r)
            it "hits back-face" $
              let r = Ray (V3 0 0 (-5)) (V3 0 0 1)
                  mh = OTRI.hittestTriangle t0 r
                  hit = fromJust mh
               in do
                    ((isJust mh) `shouldBe` True)
                    ((hitParam hit) `shouldBe` 3)
                    ((hitPos hit) `shouldBe` (V3 0 0 (-2)))
                    ((hitNorm hit) `shouldBe` zAxis)
                    ((hitInc hit) `shouldBe` r)
            it "misses back-face" $
              let mh = OTRI.hittestTriangleFront t0 (Ray (V3 0 0 (-5)) (V3 0 0 1))
               in do
                    ((isNothing mh) `shouldBe` True)

instance KD.KdTreeObject (Vec3 Double) where

  blank = origin

  pos = id

  setAxis p _ = p

kdTreeSpec :: Spec
kdTreeSpec =
  describe "KdTree" $ do
    describe "AABB" $ do
      it "Finds point cloud AABB" $
        let cloud = [V3 0 0 0, V3 1 1 1, V3 (-1) (-1) (-1), V3 0.5 0.5 0.5, V3 (-0.5) 0.5 (-0.5), V3 0.5 (-0.5) 0.5, V3 0.3 (-1) 1] :: [Vec3d]
         in do
              (KD.findAABB cloud) `shouldBe` (KD.AABB origin $ (constVec 1))
    it "sorts axes" $
      let l = [V3 1 0 (-1), V3 0 1 2, V3 2 2 1, V3 (-1) (-1) 0] :: [Vec3d]
       in do
            ((KD.sortByDim KD.XAxis l) `shouldBe` [V3 (-1) (-1) 0, V3 0 1 2, V3 1 0 (-1), V3 2 2 1])
            ((KD.sortByDim KD.YAxis l) `shouldBe` [V3 (-1) (-1) 0, V3 1 0 (-1), V3 0 1 2, V3 2 2 1])
            ((KD.sortByDim KD.ZAxis l) `shouldBe` [V3 1 0 (-1), V3 (-1) (-1) 0, V3 2 2 1, V3 0 1 2])
    it "splits points" $
      let l = [V3 x 0 0 | x <- [(-10) .. 10]] :: [Vec3d]
       in do
            (KD.splitPoints KD.XAxis l) `shouldBe` (Just origin, take 10 l, drop 11 l)
    it "balances" $
      let l = origin : [V3 x y z | x <- [(-0.2), 0.2], y <- [(-0.3), 0.3], z <- [(-0.4), 0.4]] :: [Vec3d]
       in do
            (KD.balance l) `shouldBe` (KD.KdNode KD.ZAxis (V3 0.0 0.0 0.0) (KD.KdNode KD.YAxis (V3 (-0.2) 0.3 (-0.4)) (KD.KdNode KD.XAxis (V3 0.2 (-0.3) (-0.4)) (KD.KdNode KD.ZAxis (V3 (-0.2) (-0.3) (-0.4)) KD.KdNull KD.KdNull) KD.KdNull) (KD.KdNode KD.ZAxis (V3 0.2 0.3 (-0.4)) KD.KdNull KD.KdNull)) (KD.KdNode KD.YAxis (V3 (-0.2) 0.3 0.4) (KD.KdNode KD.XAxis (V3 0.2 (-0.3) 0.4) (KD.KdNode KD.ZAxis (V3 (-0.2) (-0.3) 0.4) KD.KdNull KD.KdNull) KD.KdNull) (KD.KdNode KD.ZAxis (V3 0.2 0.3 0.4) KD.KdNull KD.KdNull)))
    it "heap orders" $
      let l = origin : [V3 x y z | x <- [(-0.2), 0.2], y <- [(-0.3), 0.3], z <- [(-0.4), 0.4]] :: [Vec3d]
       in do ((KD.kdTreeElemsBoxed $ ((KD.mkKdTreeBoxed l) :: KD.BoxedKdTree Vec3d)) `shouldBe` [V3 0.0 0.0 0.0, V3 (-0.2) 0.3 (-0.4), V3 (-0.2) 0.3 0.4, V3 0.2 (-0.3) (-0.4), V3 0.2 0.3 (-0.4), V3 0.2 (-0.3) 0.4, V3 0.2 0.3 0.4, V3 (-0.2) (-0.3) (-0.4), V3 (-0.2) (-0.3) 0.4])

photonSpec :: Spec
photonSpec =
  describe "Photon" $ do
    describe "Packing" $ do
      it "Color Packing" $
        let color = V3 0.5 0.4 0.3
            packed = 0x7c019a9a
         in do
              ((PH.packColor color) `shouldBe` packed)
              ((PH.unpackColor packed) `shouldBeApprox` color)
      it "Dir Flags Packing" $
        let dir = normalize $ V3 0.5 0.4 0.2
            flags = KD.YAxis
            packed = 0x29b67
            (d', f') = PH.unpackDirFlags packed
         in do
              ((PH.packDirFlags dir flags) `shouldBe` packed)
              (f' `shouldBe` flags)
              (assertApproxEqual 0.01 dir d')

convexHullSpec :: Spec
convexHullSpec =
  describe "Convex Hull" $ do
    describe "2D" $ do
      it "insideEdge" $ do
        ((CH.insideEdge origin xAxis yAxis zAxis) `shouldBe` True)
        ((CH.insideEdge origin xAxis (negateVec yAxis) zAxis) `shouldBe` False)
      it "lstPairs" $ do
        (CH.lstPairs [xAxis, yAxis, zAxis]) `shouldBe` [(xAxis, yAxis), (yAxis, zAxis), (zAxis, xAxis)]
      it "insertRemovePoint" $
        let pt0 = V3 1 (-1) 0
            pt1 = V3 1 1 0
            pt2 = V3 (-1) 1 0
            pt3 = V3 (-1) (-1) 0
            hull =
              [ (pt0, pt1),
                (pt1, pt2),
                (pt2, pt3),
                (pt3, pt0)
              ]
            ipt = V3 0 2 0
            hullModified =
              [ (pt0, pt1),
                (pt1, ipt),
                (ipt, pt2),
                (pt2, pt3),
                (pt3, pt0)
              ]
         in do
              ((CH.insertPoint hull origin zAxis) `shouldBe` hull)
              ((CH.insertPoint hull ipt zAxis) `shouldBe` hullModified)
      it "Convex Hull" $
        let points = [V3 x y 0 | x <- [(-1) .. 1], y <- [(-1) .. 1]] ++ hullPoints
            hullPoints =
              [ V3 2 2 0,
                V3 2 (-2) 0,
                V3 1 (-3) 0,
                V3 1 3 0,
                V3 (-2) 2 0,
                V3 (-2) (-2) 0,
                V3 (-1) (-3) 0,
                V3 (-1) 3 0
              ]
            norm = zAxis
            hullPointsOrdered =
              [ V3 1 (-3) 0,
                V3 2 (-2) 0,
                V3 2 2 0,
                V3 1 3 0,
                V3 (-1) 3 0,
                V3 (-2) 2 0,
                V3 (-2) (-2) 0,
                V3 (-1) (-3) 0
              ]
         in do
              ((fmap fst $ CH.convexHull2D points zAxis) `shouldBe` hullPointsOrdered)
              ((CH.convexHull2DArea points zAxis) `shouldBe` Just 22)
    describe "3D" $ do
      describe "splitPoints" $ do
        it "splits the points correctly" $
          let points = VS.fromList [origin, xAxis, yAxis, zAxis, negateVec zAxis]
              (inside, outside) = CH.splitPoints points (MM.mkTri 0 1 2) [0 .. (VS.length points) -1]
           in do
                outside `shouldBe` [3]
                inside `shouldBe` [0, 1, 2, 4]
      describe "pointCloudExtrema" $ do
        it "finds the extrema" $
          let points = VS.fromList ([xAxis, yAxis, zAxis, (negateVec xAxis), (negateVec yAxis), (negateVec zAxis), origin, origin, origin] :: [Vec3d])
              vecCompComp f x y = (f x) <= (f y)
              extrema = CH.pointCloudExtrema (map vecCompComp [xPos, yPos, zPos]) points
           in do extrema `shouldBe` [(3, 0), (4, 1), (5, 2)]
      describe "createSimplex" $ do
        it "errors in the empty set case" $
          let points = VS.empty
              seed = CH.createSimplex points
           in do seed `shouldBe` Nothing
        it "errors in 0d case" $
          let points = VS.fromList [origin, origin, origin, origin]
              seed = CH.createSimplex points
           in do seed `shouldBe` Nothing
        it "errors in 1d case" $
          let points = VS.fromList [origin, origin, xAxis, (negateVec xAxis)]
              seed = CH.createSimplex points
           in do seed `shouldBe` Nothing
        it "errors in 2d case" $
          let points = VS.fromList [origin, origin, xAxis, (negateVec xAxis), yAxis, (negateVec yAxis)]
              seed = CH.createSimplex points
           in do seed `shouldBe` Nothing
        it "finds maximal seed simplex" $
          let points = VS.fromList [origin, origin, origin, xAxis, yAxis, zAxis, (negateVec xAxis), (negateVec yAxis), (negateVec zAxis)]
              seed = CH.createSimplex points
           in do
                seed `shouldBe` (Just (6, 3, 4, 5))
      describe "calculateHorizon" $ do
        it "finds the horizon" $ do
          let points = VS.fromList [xAxis, yAxis, zAxis, negateVec xAxis, negateVec yAxis, negateVec zAxis, 0.5 *| (negateVec zAxis), 0.75 *| (negateVec zAxis), origin, origin, origin]
              (edges, pts) = runST $
                do
                  qm@(CH.QM {CH.qmMesh = mesh}) <- CH.newQhMesh points
                  _ <- mapM (\(tri, os) -> CH.addTri qm tri os) [(MM.mkTri 0 1 2, []), (MM.mkTri 2 1 3, []), (MM.mkTri 3 1 0, [5, 6, 7]), (MM.mkTri 0 2 3, [4])]
                  CH.calculateHorizon qm 5 (MM.mkTri 3 1 0)
              correctEdges = [(0, 3), (3, 1), (1, 0)]
           in do
                (sort pts) `shouldBe` [6, 7]
                (edges \\ correctEdges) `shouldBe` []
                (correctEdges \\ edges) `shouldBe` []
      describe "quickhull3DStep" $ do
        it "works on an empty mesh" $
          let points = VS.fromList [xAxis, yAxis, zAxis, negateVec xAxis, (V3 0 (-1) (0.1)), origin, origin, origin]
              simplex = (3, 2, 0, 1)
              seed = [MM.mkTri 0 1 2, MM.mkTri 0 2 3, MM.mkTri 0 3 1, MM.mkTri 1 3 2]
              allPts = [0 .. (VS.length points) -1]
              (Left (nextTri, nextOs)) = runST $
                do
                  qm <- CH.newQhMesh points
                  CH.quickhull3DStep qm seed allPts
              correctTri = MM.mkTri 0 2 3
              correctOutsideSet = [4]
           in do
                nextOs `shouldBe` correctOutsideSet
                nextTri `shouldBe` correctTri
      describe "quickhull3DIterate" $ do
        it "works" $
          let points = VS.fromList [xAxis, yAxis, zAxis, negateVec xAxis, negateVec yAxis, negateVec zAxis, 0.5 *| (negateVec zAxis), 0.75 *| (negateVec zAxis), 0.5 *| (negateVec yAxis), origin, origin, origin]
              (Left (nextTri, nextOs)) = runST $
                do
                  qm@(CH.QM {CH.qmMesh = mesh, CH.qmOutsideSets = osRef}) <- CH.newQhMesh points
                  _ <- mapM (\(tri, os) -> CH.addTri qm tri os) [(MM.mkTri 0 1 2, []), (MM.mkTri 2 1 3, []), (MM.mkTri 3 1 0, [5, 6, 7]), (MM.mkTri 0 2 3, [4, 8])]
                  CH.quickhull3DIterate qm (MM.mkTri 3 1 0, [5, 6, 7])
              correctTri = MM.mkTri 0 2 3
              correctOutsideSet = [4, 8]
           in do
                nextTri `shouldBe` correctTri
                nextOs `shouldBe` correctOutsideSet
        it "works multiple times" $
          let points = VS.fromList [xAxis, yAxis, zAxis, negateVec xAxis, negateVec yAxis, negateVec zAxis, 0.5 *| (negateVec zAxis), 0.75 *| (negateVec zAxis), 0.5 *| (negateVec yAxis), origin, origin, origin]
              polys = runST $
                do
                  qm@(CH.QM {CH.qmMesh = mesh, CH.qmOutsideSets = osRef}) <- CH.newQhMesh points
                  _ <- mapM (\(tri, os) -> CH.addTri qm tri os) [(MM.mkTri 0 1 2, []), (MM.mkTri 2 1 3, []), (MM.mkTri 3 1 0, [5, 6, 7]), (MM.mkTri 0 2 3, [4, 8])]
                  (Left a) <- CH.quickhull3DIterate qm (MM.mkTri 3 1 0, [5, 6, 7])
                  _ <- CH.quickhull3DIterate qm a
                  MM.enumerateTris mesh
           in do
                polys `shouldBe` [MM.mkTri 0 1 2, MM.mkTri 0 2 4, MM.mkTri 0 4 5, MM.mkTri 0 5 1, MM.mkTri 1 3 2, MM.mkTri 1 5 3, MM.mkTri 2 3 4, MM.mkTri 3 5 4]
      describe "quickhull3D" $ do
        it "finds the convex hull" $
          let points = VS.fromList [xAxis, yAxis, zAxis, negateVec xAxis, negateVec yAxis, negateVec zAxis, origin, origin, origin]
              correct = [MM.mkTri 0 1 2, MM.mkTri 2 1 3, MM.mkTri 3 1 5, MM.mkTri 5 1 0, MM.mkTri 0 2 4, MM.mkTri 2 3 4, MM.mkTri 3 5 4, MM.mkTri 5 0 4]
              (Just hull) = CH.quickhull3D points
           in do (Set.fromList hull) `shouldBe` (Set.fromList correct)
      describe "convexHull3DVolume" $ do
        it "works as expected" $
          let points = VS.fromList [xAxis, yAxis, zAxis, negateVec xAxis, negateVec yAxis, negateVec zAxis, origin, origin, origin]
              correct = [MM.mkTri 0 1 2, MM.mkTri 2 1 3, MM.mkTri 3 1 5, MM.mkTri 5 1 0, MM.mkTri 0 2 4, MM.mkTri 2 3 4, MM.mkTri 3 5 4, MM.mkTri 5 0 4]
              vol = CH.convexHull3DVolume points
           in do vol `shouldBe` (Just (8 / 6))
        it "works on large set" $
          do
            stdgen <- getStdGen
            let (Just vol) = CH.convexHull3DVolume $ VS.fromList $ take 10000 $ randoms stdgen
             in (assertApproxEqualS 0.01 vol ((4.0 / 3) * pi))

stHeapSpec :: Spec
stHeapSpec =
  describe "ST Heap" $ do
    it "Partially filled" $
      let lst = runST $ do
            heap <- STH.mkSTHeap 10 (origin :: (Vec3 Int)) (distance2 origin)
            STH.pushHeap (constVec 4) heap
            STH.pushHeap (constVec 1) heap
            STH.pushHeap (constVec 2) heap
            STH.pushHeap (constVec 6) heap
            STH.pushHeap (constVec 5) heap
            STH.getHeapContents heap
       in do
            (lst `shouldBe` ([constVec 6, constVec 5, constVec 2, constVec 1, constVec 4], 5))
    it "Fully filled" $
      let lst = runST $ do
            heap <- STH.mkSTHeap 7 (origin :: (Vec3 Int)) (distance2 origin)
            STH.pushHeap (constVec 4) heap
            STH.pushHeap (constVec 1) heap
            STH.pushHeap (constVec 2) heap
            STH.pushHeap (constVec 6) heap
            STH.pushHeap (constVec 5) heap
            STH.pushHeap (constVec 5) heap
            STH.pushHeap (constVec 5) heap
            STH.pushHeap (constVec 5) heap
            STH.pushHeap (constVec 5) heap
            STH.pushHeap (constVec 5) heap
            STH.getHeapContents heap
       in do
            (lst `shouldBe` ([constVec 5, constVec 5, constVec 5, constVec 1, constVec 4, constVec 2, constVec 5], 7))

triMeshSpec :: Spec
triMeshSpec =
  describe "TriMesh" $ do
    it "Merges" $
      let tm0 = TMesh.new (VS.fromList [V3 0 0 0, V3 1 1 1, V3 1 1 0]) (VB.fromList [MM.mkTri 0 1 2])
          tm1 = TMesh.new (VS.fromList [V3 3 0 0, V3 3 1 1, V3 3 1 0]) (VB.fromList [MM.mkTri 0 1 2])
          merged = TMesh.merge [tm0, tm1]
       in do
            (VB.toList $ TMesh.tris merged) `shouldBe` [MM.mkTri 0 1 2, MM.mkTri 3 4 5]

utilSpec :: Spec
utilSpec =
  describe "Util" $ do
    describe "interleave" $ do
      it "works" $
        let l0 = [0, 1, 2, 3]
            l1 = [4, 5]
         in do
              (Misc.interleave l0 l1) `shouldBe` [0, 4, 1, 5, 2, 3]
              (Misc.interleave l1 l0) `shouldBe` [4, 0, 5, 1, 2, 3]
              (Misc.interleave [] l0) `shouldBe` l0
              (Misc.interleave l0 []) `shouldBe` l0
              (Misc.interleave [] []) `shouldBe` ([] :: [()])
    describe "MList" $ do
      it "fromList" $ do
        let l0 = [0, 1, 2, 3]
            (l1, len) = runST $
              do
                l <- MList.fromList l0
                l' <- MList.toList l
                len <- MList.length l
                return (l', len)
         in do
              l1 `shouldBe` l0
              len `shouldBe` 4
      it "inserts" $ do
        let l0 = [0, 1, 2, 3]
            l1 = runST $
              do
                l <- MList.fromList l0
                MList.insertBefore l 8
                MList.insertAfter l 9
                MList.toList l
            l2 = runST $
              do
                l <- MList.new
                MList.insertAfter l 1
                MList.toList l
            l3 = runST $
              do
                l <- MList.new
                MList.insertBefore l 1
                MList.toList l
         in do
              l1 `shouldBe` [0, 9, 1, 2, 3, 8]
              l2 `shouldBe` [1]
              l3 `shouldBe` [1]
      it "erases" $ do
        let l0 = [0, 1, 2, 3]
            (l1, v, v2) = runST $
              do
                l <- MList.fromList l0
                (v, l) <- MList.erase l
                v2 <- MList.readRef l
                l1 <- MList.toList l
                return (l1, v, v2)
            l2 = runST $
              do
                l <- MList.singleton 0
                (_, l) <- MList.erase l
                MList.toList l
         in do
              v `shouldBe` 0
              v2 `shouldBe` 1
              l1 `shouldBe` [1, 2, 3]
              l2 `shouldBe` []

opticsSpec :: Spec
opticsSpec =
  describe "Optics" $ do
    it "refract" $
      let s2 = sqrt 2
          toSrc = V3 (-s2) s2 0
          refr = Optics.refract toSrc yAxis 0.9 1.0
          refr' = V3 0.636396 (-0.771362) 0
       in do
            refr `shouldBeApprox` refr
