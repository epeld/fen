module TheoreticalTest where
import Test.HUnit.Base
import Test.HUnit.Text

import Data.List (partition)

import TestUtils

import qualified Stepping
import qualified Piece
import qualified Color

import Square (Square(Square), string)
import Theoretical


run :: IO Counts
run = runTestTT tests

tests :: Test
tests = TestLabel "Theoretical Tests" testList

testList :: Test
testList = TestList [testStep, testOfficerRange, testPawnRange]

testStep :: Test
testStep = let squares = step a1 Stepping.right
           in TestLabel "test step" $ TestCase $ do
             assertEqual "num steps" 7 (length squares)
             assertElem "element of squares" c1 squares
             assertBool "last step" (last squares == h1)

testOfficerRange :: Test
testOfficerRange = TestLabel "officer range" $
                   TestList [testKingRange,
                             testBishopRange,
                             testKnightRange,
                             testRookRange,
                             testQueenRange]

testQueenRange :: Test
testQueenRange = TestLabel "queen range" $ TestCase $ do
  let r = officerRange Piece.Queen e4
  let sqs = concat r
  assertEqual "number of directions" 8 (length r)
  assertEqual "number of squares" (3 + 4 + 3 + 4 + 3 + 4 + 3 + 3) (length sqs)
  assertBool "initial square not in range" $ not $ elem e4 sqs
  assertElem "" a4 sqs
  assertElem "" c2 sqs
  assertElem "" h1 sqs

testRookRange :: Test
testRookRange = TestLabel "rook range" $ TestCase $ do
  let r = officerRange Piece.Rook a1
  let (empty, nonempty) = partition (== []) r
  assertEqual "number of empty directions" 2 (length empty)
  assertEqual "number of valid directions" 2 (length nonempty)
  let sqs = concat nonempty
  assertElem "" a4 sqs
  assertElem "" h1 sqs
  assertBool "reaches full rank/file" $ all (\x -> length x == 7) nonempty

testKnightRange :: Test
testKnightRange = TestLabel "knight range" $
                  TestList [testKnightCenter, testKnightEdge, testKnightCorner]

testKnightCenter :: Test
testKnightCenter = makeKnightTest e4 [c3,d2,f2,g3,g5,f6,d6,c5]

testKnightCorner :: Test
testKnightCorner = makeKnightTest a4 [b2,c3,c5,b6]

testKnightEdge :: Test
testKnightEdge = makeKnightTest a1 [b3,c2]

makeKnightTest :: Square -> [Square] -> Test
makeKnightTest sq sqs = TestCase $ do
  let r = concat $ officerRange Piece.Knight sq
  assertEqual "number of squares" (length sqs) (length r)
  assertElems "reached squares" sqs r

testPawnRange :: Test
testPawnRange = TestLabel "pawn range" $
                TestList [testWhitePawnTakes,
                          testBlackPawnTakes,
                          testBlackPawnTakesEdge,
                          testWhitePawnMoves,
                          testWhitePawnMovesLong,
                          testBlackPawnMoves,
                          testBlackPawnMovesLong]


testWhitePawnTakes :: Test
testBlackPawnTakes :: Test
testBlackPawnTakesEdge :: Test

testWhitePawnMoves :: Test
testWhitePawnMovesLong :: Test

testBlackPawnMoves = makeTestPawnRange Moves Color.Black e3 [e2]
testBlackPawnMovesLong = makeTestPawnRange Moves Color.Black e7 [e5, e6]
testWhitePawnMoves = makeTestPawnRange Moves Color.White e3 [e4]
testWhitePawnMovesLong = makeTestPawnRange Moves Color.White e2 [e3, e4]

testWhitePawnTakes = makeTestPawnRange Takes Color.White e4 [d5, f5]
testBlackPawnTakes = makeTestPawnRange Takes Color.Black e4 [d3, f3]
testBlackPawnTakesEdge = makeTestPawnRange Takes Color.Black a4 [b3]

makeTestPawnRange :: MoveType -> Color.Color -> Square -> [Square] -> Test
makeTestPawnRange mt clr src sqs =
  TestLabel (unwords [show clr, string src, "pawn", show mt, show sqs]) $
  TestCase $ do
    let r = concat $ pawnRange mt clr src
    sequence_ [assertElem "square in range" sq r | sq <- sqs]
    assertEqual "square count " (length sqs) (length r)

testKingRange :: Test
testKingRange = let r = officerRange Piece.King e4
                in TestLabel "king range" $ TestCase $ do
                  assertEqual "king reaches one square in each direction" 8 (length r)

testBishopRange = let r = officerRange Piece.Bishop a1
                      r' = filter (not. null) r
                  in TestLabel "bishop range" $ TestCase $ do
                    assertEqual "bishop can go in one direction from a1" 1 (length r')
                    assertEqual "bishop reaches h8 from a1" h8 (last (head r'))
                    assertEqual "bishop reaches 7 squares from a1" 7 (length $ head r')


b2 = Square 'b' 2
b6 = Square 'b' 6
c2 = Square 'c' 2
c3 = Square 'c' 3
d2 = Square 'd' 2
f2 = Square 'f' 2
g3 = Square 'g' 3
g5 = Square 'g' 5
f6 = Square 'f' 6
d6 = Square 'd' 6
c5 = Square 'c' 5


a4 = Square 'a' 4
b3 = Square 'b' 3

f5 = Square 'f' 5
d5 = Square 'd' 5

f3 = Square 'f' 3
d3 = Square 'd' 3

e7 = Square 'e' 7
e6 = Square 'e' 6
e5 = Square 'e' 5
e4 = Square 'e' 4
e3 = Square 'e' 3
e2 = Square 'e' 2

a1 = Square 'a' 1
c1 = Square 'c' 1
h1 = Square 'h' 1
h8 = Square 'h' 8
