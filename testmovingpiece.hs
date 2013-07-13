module TestMovingPiece where
import Test.HUnit hiding (assertFailure)

import ErrorMonad
import MovingPiece
import Square
import FEN

tests = movingPieceTests

movingPieceTests = 
    let p = startingPosition
     in TestLabel "movingPiece" $ TestList $
        map (TestCase. assertSuccessful p) pieceSquares ++
        map (TestCase. assertFailed p) emptySquares

assertSuccessful p s = assertBool msg (worked mp)
    where mp = movingPiece p s
          msg = string s ++ " success"

assertFailed p s = assertBool msg (failed mp)
    where mp = movingPiece p s
          msg = string s ++ " failure"

movingPieces p = map (movingPiece p) pieceSquares
failedMovingPieces p = map (movingPiece p) emptySquares

pieceSquares = [s | s <- squares, rank s `elem` [1,2,7,8]]
emptySquares = [s | s <- squares, not $ rank s `elem` [1,2,7,8]]

