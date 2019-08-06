-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.
import qualified Test.Tasty
-- Hspec is one of the providers for Tasty. It provides a nice syntax for
-- writing tests. Its website has more info: <https://hspec.github.io>.
import Test.Tasty.Hspec

import OpalFalcon.Math.Vector

main :: IO ()
main = do
    test <- testSpec "OpalFalcon.Math.Vector" vectorSpec
    Test.Tasty.defaultMain test

-- Only test non-trivial operations on vectors
vectorSpec :: Spec
vectorSpec = parallel $ do
    describe "Vector" $ do
        describe "approximately equal" $ do
            it "works" $ do
                ((V3 0 0 (0.3333333333 :: Double)) ~= (V3 0 0 (1/3))) `shouldBe` True
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
                ((reflect (V3 0 0 1) (normalize (V3 1 1 1))) ~= (V3 (2/3) (2/3) (-1/3))) `shouldBe` True

