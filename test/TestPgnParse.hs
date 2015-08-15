{-#LANGUAGE RankNTypes #-}
{-#LANGUAGE FlexibleContexts #-}
module TestPgnParse where
import Test.HUnit
import Test.Hspec

import Control.Lens
import Control.Monad (join)

import Data.Maybe
import Text.Parsec

import PgnParse
import ParserUtils hiding (moveType)
import Parsable
import Square
import PartialMove
import Move 
import MoveType
import Piece

import TestUtils


test = hspec $ do

    let parse p = runParser p () "test case"

    describe "PGN Parsing" $ do
        describe "Pawn Moves" $ do
            withInput "e4" pawnMove $ \mv -> do
                it "has destination e4" $ mv ^. destination `shouldBe` unsafe "e4"
                it "is a no-capture" $ mv ^. moveType `shouldBe` Moves
                it "has no source" $ mv ^. source `shouldBe` Nothing

            withInput "bxc7" pawnMove $ \mv -> do
                it "has destination c7" $ mv ^. destination `shouldBe` unsafe "c7"
                it "is a capture" $ mv ^. moveType `shouldBe` Captures
                it "has b-file as source" $ mv ^. source `shouldBe` Just (File 'b')

            withInput "b6xa7" pawnMove $ \mv -> do
                it "has destination a7" $ mv ^. destination `shouldBe` unsafe "a7"
                it "is a capture" $ mv ^. moveType `shouldBe` Captures
                it "has b6-square as source" $ mv ^. source `shouldBe` Just (Whole $ unsafe "b6")
                it "has no promotion" $ join (mv ^? promotion) `shouldBe` Nothing

            withInput "bxa8=N" pawnMove $ \mv -> do
                it "has destination a8" $ mv ^. destination `shouldBe` unsafe "a8"
                it "is a capture" $ mv ^. moveType `shouldBe` Captures
                it "has b-file as source" $ mv ^. source `shouldBe` Just (File 'b')
                it "promotes to knight" $ join (mv ^? promotion) `shouldBe` Just Knight

        describe "Officer Moves" $ do
            withInput "Bxf3" officerMove $ \mv -> do
                it "has destination f3" $ mv ^. destination `shouldBe` unsafe "f3"
                it "is a capture" $ mv ^. moveType `shouldBe` Captures
                it "has no source" $ mv ^. source `shouldBe` Nothing

withInput :: Show a => String -> Parser a -> (a -> Spec) -> Spec
withInput s p f = 
    describe label $ withRight (runParser p () label s) f
    where
    label = "Input: " ++ s
