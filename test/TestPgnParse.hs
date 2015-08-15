module TestPgnParse where
import Test.HUnit
import Test.Hspec

import Data.Maybe
import Text.Parsec

import PgnParse
import TestUtils


test = hspec $ do

    let parse p = runParser p () "test case"

    describe "PGN Parsing" $ do
        describe "Pawn Moves" $ do
            withInput "e4" pawnMove $ \r ->
                it "has destination e4" $ destination r `shouldBe` square' "e4"
                it "is a no-capture" $ moveType r `shouldBe` Moves
                it "has no hint" $ hint r `shouldBe` Nothing

            withInput "bxc7" pawnMove $ \r ->
                it "has destination c7" $ destination r `shouldBe` square' "c7"
                it "is a capture" $ moveType r `shouldBe` Takes
                it "has b-file as hint" $ hint r `shouldBe` Just (File 'b')

withInput :: String -> Parser a -> ( a -> Spec)
withInput s p f = 
    describe label $ withRight (runParser p () label s) f
    where
    label = "Input: " ++ s
