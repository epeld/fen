module RangeTest where
import Test.HUnit.Base
import Test.HUnit.Text

import Data.Maybe (fromJust)

import FENPosition
import Position (Position)
import FEN (decode)
import Square
import BoundPiece
import Range
import Theoretical (MoveType(Takes, Moves))

import TestUtils

run :: IO Counts
run = runTestTT tests

tests :: Test
tests = TestLabel "FEN Tests" testList

testList :: Test
testList = TestList [testQueenAttackRange, testQueenMoveRange]

boundQueen :: BoundPiece.Piece
boundQueen = fromJust $ bind (Square 'c' 6) samplePosition

queenAttackRange :: Range
queenAttackRange = range boundQueen Takes

queenMoveRange :: Range
queenMoveRange = range boundQueen Moves

testQueenMoveRange = TestCase $ do
  let r = queenMoveRange
  assertEqual "number of squares reached" (2 + 2 + 2 + 2 + 2 + 1 + 5) (length r)
  assertElems "reached squares" [Square 'b' 7, Square 'a' 8,
                                 Square 'b' 6, Square 'a' 6,
                                 Square 'd' 6, Square 'e' 6,
                                 Square 'd' 5,
                                 Square 'b' 5, Square 'a' 4,
                                 Square 'd' 7, Square 'e' 8,
                                 Square 'c' 5, Square 'c' 4, Square 'c' 3,
                                 Square 'c' 2, Square 'c' 1] r

testQueenAttackRange = TestCase $ do
  let r = queenAttackRange
  assertEqual "number of squares attacked" 2 (length r)
  assertElem "attacks pawn" (Square 'f' 6) r
  assertElem "attacks knight" (Square 'e' 4) r

samplePosition :: Position
samplePosition = fromJust $ decode $ sampleFen

sampleFen :: String
sampleFen =  "8/2K5/2Q2p2/4r3/4n3/5k2/8/8 w - - 7 53"
