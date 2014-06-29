module RangeTest where
import Test.HUnit.Base
import Test.HUnit.Text

import Data.Maybe (fromJust)

import FENPosition
import Position (Position, nextTurn)
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
testList = TestList [testQueenPosition, testPassantPosition]

testQueenPosition :: Test
testQueenPosition = TestLabel "Queen Position" $
                    TestList [testQueenAttackRange, testQueenMoveRange]

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

boundPawn :: BoundPiece.Piece
boundPawn = fromJust $ bind (Square 'e' 5) samplePassantPosition

pawnAttackRange :: Range
pawnAttackRange = range boundPawn Takes

pawnMoveRange :: Range
pawnMoveRange = range boundPawn Moves

testPassantPosition = TestLabel "passant position" $
                      TestList [testPawnAttackRange,
                                testPawnMoveRange,
                                testPawnCannotMoveTwoIfObstructed]

testPawnAttackRange :: Test
testPawnAttackRange = TestCase $ do
  assertEqual "Can only attack passant square" [Square 'd' 6] pawnAttackRange

testPawnMoveRange = TestCase $ do
  assertEqual "Can move forward 1 square" [Square 'e' 6] pawnMoveRange

testPawnCannotMoveTwoIfObstructed = TestCase $ do
  let p = nextTurn samplePassantPosition
  let r = range (fromJust $ bind (Square 'e' 7) p) Moves
  assertEqual "reaches only e6" [Square 'e' 6] r

samplePosition :: Position
samplePosition = fromJust $ decode sampleFen

samplePassantPosition :: Position
samplePassantPosition = fromJust $ decode samplePassantFen

sampleFen :: String
sampleFen =  "8/2K5/2Q2p2/4r3/4n3/5k2/8/8 w - - 7 53"

samplePassantFen :: String
samplePassantFen = "r1bqkbnr/ppp1pppp/2n5/3pP3/8/8/PPPP1PPP/RNBQKBNR w KkQq d6 0 4"
