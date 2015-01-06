module TestSquare where
import Test.Hspec
import Test.QuickCheck
import Control.Applicative

import Square

main :: IO ()
main = hspec $ do
    describe "Square" $ do

        describe "square" $ do
            it "can read the square e4" $ do
                square "e4" `shouldBe` Just (Square (5, 4))

            it "can read the square a1" $ do
                square "a1" `shouldBe` Just (Square (1, 1))

            it "can read the square a8" $ do
                square "a8" `shouldBe` Just (Square (1, 8))


        describe "string" $ do
            it "can encode h8" $ do
                string (Square (8, 8)) `shouldBe` "h8"

            it "can encode d5" $ do
                string (Square (4, 5)) `shouldBe` "d5"


        describe "diff" $ do
            it "between e4 and e2" $ do
                diff (unsafe "e4") (unsafe "e2") `shouldBe` (0, 2)

            it "between e4 and f3" $ do
                diff (unsafe "e4") (unsafe "f3") `shouldBe` (-1, 1)


        describe "add" $ do
            it "a1 and (5,3)" $ do
                add (unsafe "a1") (5, 0) `shouldBe` square "f1"


        describe "properties" $ do
            it "unsafe is inverse of string" $ do
                let coords = (,) <$> [1..8] <*> [1..8]
                    sq = Square
                    deenc = unsafe. string
                forAll (elements coords) $ \ab
                    -> deenc (sq ab) == id (sq ab)
