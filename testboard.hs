module TestBoard where
import Test.HUnit
import Control.Monad.State (runState)

import Board
import qualified FEN
import Position
import Piece
import Square
import Color

initialBoard = board FEN.startingPosition

tests = TestList [
    replaceTests,
    moveTests
    ]

moveTests = TestLabel "test move " $ TestCase testMove

replaceTests= TestLabel "test replace" $ TestList [
    TestCase testReplace,
    TestCase testReplace2
    ]

whiteQueen = Piece (Officer Queen) White 
whitePawn = Piece (Officer Queen) White 
e4 = square' 'e' 4
e2 = square' 'e' 4

testReplace = do
    let b = replace e4 (Just whiteQueen) initialBoard
    assertEqual "queen at e4" (Just whiteQueen) (b !!! e4)

testReplace2 = do
    let b = replace e2 (Just whiteQueen) initialBoard
    assertEqual "queen at e2" (Just whiteQueen) (b !!! e2)

testMove = do
    let (_, b) = runState (move e2 e4) initialBoard
    assertEqual "pawn at e4" (Just whitePawn) (b !!! e4)

