module FunctionalTests where
import Test.HUnit

import Square
import MoveUtils
import Position
import FEN

-- TODO test:
-- cant capture king
-- does promote
-- cant castle through pieces
-- cant castle in check
-- captured passant pawn dissapears
-- right knight is selected when multiple choice

testWhitePassant =
    let p = positionAfter startingPosition "e4"
     in assertEqual "white passant" (Just $ square' 'e' 3) (enPassant p)

testBlackPassant =
    let p = positionAfter startingPosition "e4 e5"
     in assertEqual "black passant" (Just $ square' 'e' 6) (enPassant p)
