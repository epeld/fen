{-# LANGUAGE RankNTypes #-}
module HelperReaders where
import Test.HUnit
import Test.Hspec

import TestUtils

import Control.Monad.Reader
import Data.Maybe
import Data.Char (toLower)

import Piece
import Square
import Position

hasKnightAt :: Position -> String -> Spec
hasKnightAt = hasOfficerAt Knight

hasBishopAt :: Position -> String -> Spec
hasBishopAt = hasOfficerAt Bishop

hasKingAt :: Position -> String -> Spec
hasKingAt = hasOfficerAt King

hasRookAt :: Position -> String -> Spec
hasRookAt = hasOfficerAt Rook

hasQueenAt :: Position -> String -> Spec
hasQueenAt = hasOfficerAt Queen

hasOfficerAt :: OfficerType -> Position -> String -> Spec
hasOfficerAt t p s = pieceSatisfyingAt p (\pc -> pieceType pc == Officer t) s label
    where
    label = unwords ["has", fmap toLower (show t)]

hasPawnAt :: Position -> String -> Spec
hasPawnAt p s = pieceSatisfyingAt p (\pc -> pieceType pc == Pawn) s "has pawn"

pieceSatisfyingAt :: Position -> (Piece -> Bool) -> String -> String -> Spec
pieceSatisfyingAt p f s txt = 
    describe ("square " ++ s) $ do
        let pc = pieceAt p (unsafe s)
        it "is occupied" (assertJust pc)
        withJust' pc $ \p ->
            it txt (p `shouldSatisfy` f)
