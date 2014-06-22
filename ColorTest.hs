module ColorTest where
import Test.HUnit.Base
import Test.HUnit.Text

import Color

run :: IO Counts
run = runTestTT tests

tests :: Test
tests = TestLabel "Color Tests" testList

testList :: Test
testList = TestList [testToggle, testLastRank, testInitialRank]

testToggle :: Test
testToggle = TestList [testToggleWhite, testToggleBlack]

testToggleWhite :: Test
testToggleWhite = TestCase $ assertEqual "toggle white" Black (toggle White)

testToggleBlack :: Test
testToggleBlack = TestCase $ assertEqual "toggle white" White (toggle Black)

testLastRank :: Test
testLastRank = TestList [testLastRankWhite, testLastRankBlack]

testLastRankWhite :: Test
testLastRankWhite = TestCase $
                    assertEqual "last rank of white is 8" 8 (lastRank White)

testLastRankBlack :: Test
testLastRankBlack = TestCase $
                    assertEqual "last rank of black is 1" 1 (lastRank Black)


testInitialRank :: Test
testInitialRank = TestList [testInitialRankWhite, testInitialRankBlack]

testInitialRankWhite :: Test
testInitialRankWhite = TestCase $
                    assertEqual "initial rank of white is 2" 2 (initialRank White)

testInitialRankBlack :: Test
testInitialRankBlack = TestCase $
                    assertEqual "initial rank of black is 7" 7 (initialRank Black)
