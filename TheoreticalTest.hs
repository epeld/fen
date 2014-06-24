module TheoreticalTest where
import Test.HUnit.Base
import Test.HUnit.Text

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
             assertEqual "7 steps" 7 (length squares)
             assertBool "c1 in steps" (elem c1 squares)
             assertBool "last step = h1" (last squares == h1)

testOfficerRange :: Test
testOfficerRange = TestLabel "officer range" $
                   TestList [testKingRange, testBishopRange]

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

assertElem :: (Show a, Eq a) => String -> a -> [a] -> Assertion
assertElem s a as =
  let s' = unwords [s, ". Expected:", show a, "in", show as]
  in assertBool s' (elem a as)

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
