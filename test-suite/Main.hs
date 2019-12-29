-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.

-- Hspec is one of the providers for Tasty. It provides a nice syntax for
-- writing tests. Its website has more info: <https://hspec.github.io>.

import Control.DeepSeq
import Control.Exception as E
import Control.Monad
import Data.CallStack
import OpalFalcon.Math
import OpalFalcon.Math.Matrix
import OpalFalcon.Math.Transformations
import OpalFalcon.Math.Vector
import OpalFalcon.Scene.Camera
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
  vectorTests <- testSpec "OpalFalcon.Math.Vector" vectorSpec
  matrixTests <- testSpec "OpalFalcon.Math.Matrix" matrixSpec
  transformationsTests <- testSpec "OpalFalcon.Math.Transformations" transformationsSpec
  cameraTests <- testSpec "OpalFalcon.Scene.Camera" cameraSpec
  Test.Tasty.defaultMain $ Test.Tasty.testGroup "OpalFalcon" [vectorTests, matrixTests, transformationsTests, cameraTests]

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
