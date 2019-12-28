-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.

-- Hspec is one of the providers for Tasty. It provides a nice syntax for
-- writing tests. Its website has more info: <https://hspec.github.io>.

import OpalFalcon.Math.Vector
import OpalFalcon.Math.Matrix

import qualified Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
  vectorTests <- testSpec "OpalFalcon.Math.Vector" vectorSpec
  matrixTests <- testSpec "OpalFalcon.Math.Matrix" matrixSpec
  Test.Tasty.defaultMain $ Test.Tasty.testGroup "OpalFalcon" [vectorTests, matrixTests]

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
        -- TODO: it would be nice to extend HUnit to support "approximately equal" for the sake
        --  of floating point operations (especailly matrix transforms)
        ((reflect (V3 0 0 1) (normalize (V3 1 1 1))) ~= (V3 (2 / 3) (2 / 3) (-1 / 3))) `shouldBe` True

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
