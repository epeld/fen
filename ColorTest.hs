module ColorTest where
import Test.HUnit.Base
import Test.HUnit.Text

import Color

run = runTestTT tests

tests = TestLabel "Color Tests" testList

testList = TestList [testToggle, testLastRank, testInitialRank]

testToggle = TestList [testToggleWhite, testToggleBlack]
testToggleWhite = TestCase (assertEqual "toggle white" Black (toggle White))
testToggleBlack = TestCase (assertEqual "toggle white" White (toggle Black))

testLastRank = TestList [testLastRankWhite, testLastRankBlack]
testLastRankWhite = TestCase $
                    assertEqual "last rank of white is 8" 8 (lastRank White)
testLastRankBlack = TestCase $
                    assertEqual "last rank of black is 1" 1 (lastRank Black)


testInitialRank = TestList [testInitialRankWhite, testInitialRankBlack]
testInitialRankWhite = TestCase $
                    assertEqual "initial rank of white is 2" 2 (initialRank White)
testInitialRankBlack = TestCase $
                    assertEqual "initial rank of black is 7" 7 (initialRank Black)
