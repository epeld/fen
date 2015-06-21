{-# LANGUAGE RankNTypes #-}
module HelperReaders where
import Test.HUnit
import Test.Hspec

import TestUtils

import Control.Monad.Reader
import Data.Maybe

import Piece
import Square
import Position

hasPawnAt :: Position -> String -> Spec
hasPawnAt p s = pieceSatisfyingAt p (\pc -> pieceType pc == Pawn) s "has pawn"

pieceSatisfyingAt :: Position -> (Piece -> Bool) -> String -> String -> Spec
pieceSatisfyingAt p f s txt = 
    describe ("square " ++ s) $ do
        let pc = pieceAt p (square' s)
        it "is occupied" (assertJust pc)
        withJust' pc $ \p ->
            it txt (p `shouldSatisfy` f)
