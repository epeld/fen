module TheoreticalTest where
import Test.HUnit.Base
import Test.HUnit.Text

import qualified Stepping
import qualified Piece
import Square (Square(Square))
import Theoretical

run :: IO Counts
run = runTestTT tests

tests :: Test
tests = TestLabel "Theoretical Tests" testList

testList :: Test
testList = TestList [testStep, testOfficerRange]

testStep :: Test
testStep = let squares = step a1 Stepping.right
           in TestCase $ do
             assertEqual "7 steps" 7 (length squares)
             assertBool "c1 in steps" (elem c1 squares)
             assertBool "last step = h1" (last squares == h1)

testOfficerRange :: Test
testOfficerRange = TestList [testKingRange, testBishopRange]

testPawnRange :: Test
testPawnRange = TestList [testWhitePawnTakes, testBlackPawnTakes, testPawnMovesLong, testPawnMovesShort]

testKingRange :: Test
testKingRange = let r = officerRange Piece.King e4
                in TestCase $ do
                  assertEqual "king reaches one square in each direction" 8 (length r)

testBishopRange = let r = officerRange Piece.Bishop a1
                      r' = filter (not. null) r
                  in TestCase $ do
                    assertEqual "bishop can go in one direction from a1" 1 (length r')
                    assertEqual "bishop reaches h8 from a1" h8 (last (head r'))
                    assertEqual "bishop reaches 7 squares from a1" 7 (length $ head r')

e4 = Square 'e' 4
a1 = Square 'a' 1
c1 = Square 'c' 1
h1 = Square 'h' 1
h8 = Square 'h' 8
