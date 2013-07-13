module TestSquare where
import Test.HUnit
import Data.Maybe (fromJust)

import Square
import TestUtils

tests = TestLabel "Square Tests" testList

testList = TestList [
            TestLabel "internal square tests" $ Square.internalTests,
            TestLabel "square creation" squareTests,
            TestLabel "horizontal series" $ TestCase testHorizontalSeries,
            TestLabel "horizontal series backward" $
                TestCase testHorizontalSeries2,
            TestLabel "diagonal series" $ TestCase testDiagonalSeries,
            TestLabel "diagonal series2" $ TestCase testDiagonalSeries2,
            TestLabel "diagonal series3" $ TestCase testDiagonalSeries3,
            TestLabel "file letters" $ TestCase testFileLetters,
            TestLabel "rank numbers" $ TestCase testRankNumbers,
            TestLabel "squares" $ TestCase testSquares,
            TestLabel "square indexes" $ indexesTest,
            TestCase indexTest,
            movementTests
            ]

movementTests = TestList $ map TestCase $
    [ testAbove, testBelow, testLeft, testRight ]

indexTest = let s = square' 'e' 4
             in assertEqual "index" (5 + 3*8) $ [1..] !!! s

indexesTest = TestList $ map TestCase $ map assertValid indexes
    where indexes = map ([1..] !!!) squares
          assertValid = assertBool "< 64". (64 >=)

squareTests = TestList [ TestCase testSquare, TestCase testSquareFail ]

testSquare =
    let s = square 'a' 3
        s' = fromJust $ s
     in do
        assertBool "is square" (Nothing /= s)
        assertEqual "right file" 'a' $ file s'
        assertEqual "right file" 3 $ rank s'

testSquareFail =
    let s = square 'z' 4
     in do
        assertEqual "No square" Nothing $ s

testSquares = assertLength 64 squares
testFileLetters = assertLength 8 fileLetters
testRankNumbers = assertLength 8 rankNumbers

testHorizontalSeries = 
    let start = square' 'a' 3
        end = square' 'e' 3
     in testSeries start end 5

testHorizontalSeries2 = do
    let start = square' 'e' 3
        end = square' 'a' 3
     in testSeries start end 5

testDiagonalSeries = 
    let start = square' 'a' 1
        end = square' 'c' 3
     in testSeries start end 3

testDiagonalSeries2 =
    let start = square' 'h' 8
        end = square' 'c' 3
     in testSeries start end 6

testDiagonalSeries3 = 
    let start = square' 'a' 5
        end = square' 'c' 3
     in testSeries start end 3

testSeries start end l = do
    let s = series start end
    assertLength l s
    assertEqual "first square" start $ head s
    assertEqual "last square" end $ last s

testAbove = let s = square' 'e' 4
                s' = square' 'e' 5
             in assertEqual "above" (Just s') (above s)

testBelow = let s = square' 'e' 4
                s' = square' 'e' 3
             in assertEqual "below" (Just s') (below s)

testLeft = let s = square' 'e' 4
               s' = square' 'd' 4
             in assertEqual "left" (Just s') (left s)

testRight = let s = square' 'e' 4
                s' = square' 'f' 4
             in assertEqual "right" (Just s') (right s)
