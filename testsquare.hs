import Test.HUnit
import Square

import Data.Maybe (fromJust)

tests = TestLabel "Square Tests" testList

testList = TestList [
            TestLabel "square creation" squareTests,
            TestLabel "horizontal series" $ TestCase testHorizontalSeries,
            TestLabel "file letters" $ TestCase testFileLetters,
            TestLabel "rank numbers" $ TestCase testRankNumbers,
            TestLabel "squares" $ TestCase testSquares,
            movementTests,
            TestCase indexTest,
            TestLabel "square indexes" $ indexesTest
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

testHorizontalSeries = do
    let start = square' 'a' 3
    let end = square' 'e' 3
    let s = series start end
    assertString $ show s
    assertLength 5 s
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

assertLength l a = assertEqual s l $ length a
    where s = "length " ++ show l
