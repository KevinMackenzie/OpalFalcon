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
import Data.Maybe
import OpalFalcon.BaseTypes
import OpalFalcon.KdTree
import OpalFalcon.Math
import OpalFalcon.Math.Matrix
import OpalFalcon.Math.Ray
import OpalFalcon.Math.Transformations
import OpalFalcon.Math.Vector
import OpalFalcon.Photon.Photon
import OpalFalcon.Scene.Camera
import OpalFalcon.Scene.Objects.Triangle
import Test.HUnit.Lang
import qualified Test.Tasty
import Test.Tasty.Hspec

location :: HasCallStack => Maybe SrcLoc
location = case reverse callStack of
  (_, loc) : _ -> Just loc
  [] -> Nothing

-- Like `assertEqual`, but it works with vector approximately-equal
assertApproxEqual preface var expected actual =
  unless (approxEq var actual expected) $ do
    (prefaceMsg `deepseq` expectedMsg `deepseq` actualMsg `deepseq` E.throwIO (HUnitFailure location $ ExpectedButGot prefaceMsg expectedMsg actualMsg))
  where
    prefaceMsg
      | null preface = Nothing
      | otherwise = Just preface
    expectedMsg = show expected
    actualMsg = show actual

shouldBeApprox a e = assertApproxEqual "" 0.001 e a

main :: IO ()
main = do
  kdTreeTests <- testSpec "OpalFalcon.KdTree" kdTreeSpec
  photonTests <- testSpec "OpalFalcon.Photon.Photon" photonSpec
  vectorTests <- testSpec "OpalFalcon.Math.Vector" vectorSpec
  matrixTests <- testSpec "OpalFalcon.Math.Matrix" matrixSpec
  transformationsTests <- testSpec "OpalFalcon.Math.Transformations" transformationsSpec
  cameraTests <- testSpec "OpalFalcon.Scene.Camera" cameraSpec
  triangleTests <- testSpec "OpalFalcon.Scene.Objects.Triangle" triangleSpec
  Test.Tasty.defaultMain $ Test.Tasty.testGroup "OpalFalcon" [kdTreeTests, photonTests, vectorTests, matrixTests, transformationsTests, cameraTests, triangleTests]

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
      (V3 (V3 1 4 7) (V3 2 5 8) (V3 3 6 9)) ||*|| (V3 (V3 10 13 16) (V3 11 14 17) (V3 12 15 18)) `shouldBe` (V3 (V3 84 201 318) (V3 90 216 342) (V3 96 231 366))
    it "does not commute" $
      let x = (V3 (V3 1 2 3) (V3 4 5 6) (V3 7 8 9))
          y = (V3 (V3 2 3 4) (V3 3 4 5) (V3 4 5 6))
       in do
            (x ||*|| y) `shouldNotBe` (y ||*|| x)
    it "transposes" $ do
      (transpose (V4 (V4 1 2 3 4) (V4 5 6 7 8) (V4 9 10 11 12) (V4 13 14 15 16))) `shouldBe` (V4 (V4 1 5 9 13) (V4 2 6 10 14) (V4 3 7 11 15) (V4 4 8 12 16))

-- Tests transformation semantics and non-trivial transformations
transformationsSpec :: Spec
transformationsSpec = parallel $ do
  describe "Transformations" $ do
    it "apply correctly" $ do
      (applyTransform (scale (V3 1 2 3)) (V4 1 2 3 4)) `shouldBe` (V4 1 4 9 4)
      (applyTransform3 identity (V3 1 2 3)) `shouldBe` (V3 1 2 3)
      (applyTransformStack [identity] (V4 1 2 3 4)) `shouldBe` (V4 1 2 3 4)
    it "apply in the correct order" $
      let x = (V4 (V4 1 2 3 4) (V4 5 6 7 8) (V4 9 10 11 12) (V4 13 14 15 16))
          y = (V4 (V4 11 12 13 14) (V4 15 16 17 18) (V4 19 110 111 112) (V4 113 114 115 116))
          z = x ||*|| y
          v = V4 1 2 3 4
          vt = applyTransform z v
          vt1 = foldr (applyTransform) v [x, y]
          vt2 = applyTransformStack [x, y] v
       in do
            (vt1 `shouldBe` vt)
            (vt2 `shouldBe` vt)
    it "looks at correctly" $ do
      ((lookAt (V3 0 0 1)) `shouldBe` identity)

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
      let t0 = MkTriangle (V3 (-1) (-1) (-2)) (V3 1 0 (-2)) (V3 0 1 (-2))
          tmat t _ = undefined
       in do
            it "hits front-face (from +z)" $
              let r = Ray (V3 0 0 1) (V3 0 0 (-1))
                  mh = hittestTriangle t0 tmat r
                  hit = fromJust mh
               in do
                    ((isJust mh) `shouldBe` True)
                    ((hitParam hit) `shouldBe` 3)
                    ((hitPos hit) `shouldBe` (V3 0 0 (-2)))
                    ((hitNorm hit) `shouldBe` zAxis)
                    ((hitInc hit) `shouldBe` r)
            it "hits front-face (from -z)" $
              let r = Ray (V3 0 0 (-1)) (V3 0 0 (-1))
                  mh = hittestTriangle t0 tmat r
                  hit = fromJust mh
               in do
                    ((isJust mh) `shouldBe` True)
                    ((hitParam hit) `shouldBe` 1)
                    ((hitPos hit) `shouldBe` (V3 0 0 (-2)))
                    ((hitNorm hit) `shouldBe` zAxis)
                    ((hitInc hit) `shouldBe` r)
            it "hits back-face" $
              let r = Ray (V3 0 0 (-5)) (V3 0 0 1)
                  mh = hittestTriangle t0 tmat r
                  hit = fromJust mh
               in do
                    ((isJust mh) `shouldBe` True)
                    ((hitParam hit) `shouldBe` 3)
                    ((hitPos hit) `shouldBe` (V3 0 0 (-2)))
                    ((hitNorm hit) `shouldBe` zAxis)
                    ((hitInc hit) `shouldBe` r)
            it "misses back-face" $
              let mh = hittestTriangleFront t0 tmat (Ray (V3 0 0 (-5)) (V3 0 0 1))
               in do
                    ((isNothing mh) `shouldBe` True)

instance KdTreeObject (Vec3 Double) where

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
              (findAABB cloud) `shouldBe` (AABB origin $ (constVec 1))
    it "sorts axes" $
      let l = [V3 1 0 (-1), V3 0 1 2, V3 2 2 1, V3 (-1) (-1) 0] :: [Vec3d]
       in do
            ((sortByDim XAxis l) `shouldBe` [V3 (-1) (-1) 0, V3 0 1 2, V3 1 0 (-1), V3 2 2 1])
            ((sortByDim YAxis l) `shouldBe` [V3 (-1) (-1) 0, V3 1 0 (-1), V3 0 1 2, V3 2 2 1])
            ((sortByDim ZAxis l) `shouldBe` [V3 1 0 (-1), V3 (-1) (-1) 0, V3 2 2 1, V3 0 1 2])
    it "splits points" $
      let l = [V3 x 0 0 | x <- [(-10) .. 10]] :: [Vec3d]
       in do
            (splitPoints XAxis l) `shouldBe` (Just origin, take 10 l, drop 11 l)
    it "balances" $
      let l = origin : [V3 x y z | x <- [(-0.2), 0.2], y <- [(-0.3), 0.3], z <- [(-0.4), 0.4]] :: [Vec3d]
       in do
            (balance l) `shouldBe` (KdNode ZAxis (V3 0.0 0.0 0.0) (KdNode YAxis (V3 (-0.2) 0.3 (-0.4)) (KdNode XAxis (V3 0.2 (-0.3) (-0.4)) (KdNode ZAxis (V3 (-0.2) (-0.3) (-0.4)) KdNull KdNull) KdNull) (KdNode ZAxis (V3 0.2 0.3 (-0.4)) KdNull KdNull)) (KdNode YAxis (V3 (-0.2) 0.3 0.4) (KdNode XAxis (V3 0.2 (-0.3) 0.4) (KdNode ZAxis (V3 (-0.2) (-0.3) 0.4) KdNull KdNull) KdNull) (KdNode ZAxis (V3 0.2 0.3 0.4) KdNull KdNull)))
    it "heap orders" $
      let l = origin : [V3 x y z | x <- [(-0.2), 0.2], y <- [(-0.3), 0.3], z <- [(-0.4), 0.4]] :: [Vec3d]
       in do ((kdTreeElems $ ((mkKdTree l) :: BoxedKdTree Vec3d)) `shouldBe` [V3 0.0 0.0 0.0, V3 (-0.2) 0.3 (-0.4), V3 (-0.2) 0.3 0.4, V3 0.2 (-0.3) (-0.4), V3 0.2 0.3 (-0.4), V3 0.2 (-0.3) 0.4, V3 0.2 0.3 0.4, V3 (-0.2) (-0.3) (-0.4), V3 (-0.2) (-0.3) 0.4])

photonSpec :: Spec
photonSpec =
  describe "Photon" $ do
    describe "Packing" $ do
      it "Color Packing" $
        let color = V3 0.5 0.4 0.3
            packed = 0x7c019a9a
         in do
              ((packColor color) `shouldBe` packed)
              ((unpackColor packed) `shouldBeApprox` color)
      it "Dir Flags Packing" $ 
        let dir = normalize $ V3 0.5 0.4 0.2
            flags = YAxis
            packed = 0x29b67
            (d', f') = unpackDirFlags packed
         in do 
            ((packDirFlags dir flags) `shouldBe` packed)
            (f' `shouldBe` flags)
            (assertApproxEqual "" 0.01 dir d')
