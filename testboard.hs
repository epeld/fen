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
    replaceTests, moveTests, removeTests, putTests 
    ]

moveTests = TestLabel "move " $ TestCase testMove
removeTests = TestLabel "remove" $ TestCase testRemove
putTests = TestLabel "put" $ TestCase testPut
replaceTests= TestLabel "replace" $ TestList [
    TestCase testReplace,
    TestCase testReplace2
    ]

whiteQueen = Piece (Officer Queen) White 
blackQueen =  Piece (Officer Queen) Black
whitePawn = Piece Pawn White 
e4 = square' 'e' 4
e2 = square' 'e' 2

testReplace = do
    let b = replace e4 (Just whiteQueen) initialBoard
    assertEqual "queen at e4" (Just whiteQueen) (b !!! e4)

testReplace2 = do
    let b = replace e2 (Just whiteQueen) initialBoard
    assertEqual "queen at e2" (Just whiteQueen) (b !!! e2)

testMove = do
    let (_, b) = runState (move e2 e4) initialBoard
    assertEqual "pawn at e4" (Just whitePawn) (b !!! e4)
    
testRemove = do
    let (mp, b) = runState (remove e2) initialBoard
    assertEqual "nothing at e2" Nothing (b !!! e2)
    assertEqual "pawn removed" (Just whitePawn) mp

testPut = do
    let ((), b) = runState (put blackQueen e2) initialBoard
    assertEqual "queen at e2" (Just blackQueen) (b !!! e2)
    

