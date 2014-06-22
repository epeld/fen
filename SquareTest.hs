module SquareTest where
import Test.HUnit.Base
import Test.HUnit.Text

import Data.Maybe (Maybe(Just), isNothing)

import Square

run :: IO Counts
run = runTestTT tests

tests :: Test
tests = TestLabel "Square Tests" testList

testList :: Test
testList = TestList [testCoords,
                     testAdjacent,
                     testAdjacentFiles,
                     testRanks, testFiles,
                     testSquare]

testCoords :: Test
testCoords = TestCase $
             (assertEqual "coords of Square e 4 " ('e', 4) (coords $ Square 'e' 4))

testAdjacent :: Test
testAdjacent = TestList [
  TestCase (assertBool "adjacent 10 11" (adjacent 10 11)),
  TestCase (assertBool "adjacent 'b' 'a'" (adjacent 'b' 'a'))]

testAdjacentFiles :: Test
testAdjacentFiles = let g3 = Square 'g' 3
                        h8 = Square 'h' 8
                    in TestCase $
                       (assertBool "adjacent files: g3 h8" (adjacentFiles g3 h8))

testRanks :: Test
testRanks = TestCase $ assertBool "8 ranks" (8 == length ranks)

testFiles :: Test
testFiles = TestCase $ assertBool "8 files" (8 == length files)

testSquare :: Test
testSquare = TestList [
  testSquareCreation,
  testSquareFailureLow,
  testSquareFailureHigh
  ]

testSquareCreation :: Test
testSquareCreation = TestCase $
                     (assertEqual "valid square" (Just $ Square 'a' 3) (square 'a' 3))

testSquareFailureLow :: Test
testSquareFailureLow = TestCase $
                    (assertBool "invalid square" (isNothing $ square 'a' 0))

testSquareFailureHigh :: Test
testSquareFailureHigh = TestCase $
                    (assertBool "invalid square" (isNothing $ square 'a' 9))
