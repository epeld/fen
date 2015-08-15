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
import MoveDescription (Description(..))
import Move 
import MoveType
import Piece
import Castling

import TestUtils


test = hspec $ do

    let parse p = runParser p () "test case"

    describe "PGN Parsing" $ do
        describe "Castling Moves" $ do
            withInput "O-O" castlingSide $ \s -> do
                it "is kingside" $ s `shouldBe` Kingside

            withInput "O-O-O" castlingSide $ \s -> do
                it "is queenside" $ s `shouldBe` Queenside


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
                it "has b6-square as source" $ mv ^. source `shouldBe` wholeSquare "b6"
                it "has no promotion" $ join (mv ^? promotion) `shouldBe` Nothing

            withInput "bxa8=N" pawnMove $ \mv -> do
                it "has destination a8" $ mv ^. destination `shouldBe` unsafe "a8"
                it "is a capture" $ mv ^. moveType `shouldBe` Captures
                it "has b-file as source" $ mv ^. source `shouldBe` Just (File 'b')
                it "promotes to knight" $ join (mv ^? promotion) `shouldBe` Just Knight


        describe "Officer Moves" $ do
            withInput "Qc3" officerMove $ \mv -> do
                it "has destination c3" $ mv ^. destination `shouldBe` unsafe "c3"
                it "is a non-capture" $ mv ^. moveType `shouldBe` Moves
                it "has no source" $ mv ^. source `shouldBe` Nothing

            withInput "Bxf3" officerMove $ \mv -> do
                it "has destination f3" $ mv ^. destination `shouldBe` unsafe "f3"
                it "is a capture" $ mv ^. moveType `shouldBe` Captures
                it "has no source" $ mv ^. source `shouldBe` Nothing

            withInput "Bf3xe2" officerMove $ \mv -> do
                it "has destination e2" $ mv ^. destination `shouldBe` unsafe "e2"
                it "is a capture" $ mv ^. moveType `shouldBe` Captures
                it "has f3 as source" $ mv ^. source `shouldBe` wholeSquare "f3"


        describe "Move Classification" $ do
            let qc3 = OfficerMove (Description Nothing c3 Moves) Queen
                dxe3 = PawnMove (Description (Just (File 'd')) e3 Captures) Nothing
                e3 =  unsafe "e3"
                c3 = unsafe "c3"
                
            withInput "Qc3" move $ \mv -> do
                it "is a queen move to c3" $ mv `shouldBe` Right qc3

            withInput "O-O-O" move $ \mv -> do
                it "is a queenside castling move" $ mv `shouldBe` Left Queenside

            withInput "dxe3" move $ \mv -> do
                it "is a pawn capture on e3" $ mv `shouldBe` Right dxe3

withInput :: Show a => String -> Parser a -> (a -> Spec) -> Spec
withInput s p f = 
    describe label $ withRight (runParser p () label s) f
    where
    label = "Input: " ++ s


wholeSquare = fmap Whole . Square.square
