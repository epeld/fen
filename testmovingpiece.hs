module TestMovingPiece where
import Test.HUnit hiding (assertFailure)

import ErrorMonad
import MovingPiece
import Square
import FEN

tests = TestList [movingPieceTests, friendlyTests]

friendlyTests =
    let p = startingPosition
     in TestLabel "friendly" $ TestList $
        map (TestCase. assertFriendly p) whiteSquares ++
        map (TestCase. assertFriendlyFailed p) blackSquares

assertFriendly p s = assertBool msg (worked mp)
    where mp = friendly p s
          msg = string s ++ " success"

assertFriendlyFailed p s = assertBool msg (failed mp)
    where mp = friendly p s
          msg = string s ++ " failed"

movingPieceTests = 
    let p = startingPosition
     in TestLabel "movingPiece" $ TestList $
        map (TestCase. assertMP p) pieceSquares ++
        map (TestCase. assertMPFailed p) emptySquares

assertMP p s = assertBool msg (worked mp)
    where mp = movingPiece p s
          msg = string s ++ " success"

assertMPFailed p s = assertBool msg (failed mp)
    where mp = movingPiece p s
          msg = string s ++ " failure"

movingPieces p = map (movingPiece p) pieceSquares
failedMovingPieces p = map (movingPiece p) emptySquares

pieceSquares = [s | s <- squares, rank s `elem` [1,2,7,8]]
emptySquares = [s | s <- squares, not $ rank s `elem` [1,2,7,8]]
whiteSquares = [s | s <- squares, rank s `elem` [1,2]]
blackSquares = [s | s <- squares, rank s `elem` [7,8]]


