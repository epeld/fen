module SquareTest where
import Test.HUnit.Base
import Test.HUnit.Text

import Data.Maybe (Maybe(Just), isNothing)

import Square

run = runTestTT tests

tests = TestLabel "Square Tests" testList

testList = TestList [testCoords,
                     testAdjacent,
                     testAdjacentFiles,
                     testRanks, testFiles,
                     testSquare]

testCoords = TestCase $
             (assertEqual "coords of Square e 4 " ('e', 4) (coords $ Square 'e' 4))

testAdjacent = TestList [
  TestCase (assertBool "adjacent 10 11" (adjacent 10 11)),
  TestCase (assertBool "adjacent 'b' 'a'" (adjacent 'b' 'a'))]

testAdjacentFiles = let g3 = Square 'g' 3
                        h8 = Square 'h' 8
                    in TestCase $
                       (assertBool "adjacent files: g3 h8" (adjacentFiles g3 h8))

testRanks = TestCase $ assertBool "8 ranks" (8 == length ranks)
testFiles = TestCase $ assertBool "8 files" (8 == length files)

testSquare = TestList [
  testSquareCreation,
  testSquareFailureLow,
  testSquareFailureHigh
  ]

testSquareCreation = TestCase $
                     (assertEqual "valid square" (Just $ Square 'a' 3) (square 'a' 3))

testSquareFailureLow = TestCase $
                    (assertBool "invalid square" (isNothing $ square 'a' 0))

testSquareFailureHigh = TestCase $
                    (assertBool "invalid square" (isNothing $ square 'a' 9))
