module FENTest where
import Test.HUnit.Base
import Test.HUnit.Text

import Data.Maybe (fromJust)
import Data.Set as Set

import Color
import Square
import FEN
import Position
import Castling
import FENPosition


run :: IO Counts
run = runTestTT tests

tests :: Test
tests = TestLabel "FEN Tests" testList

testList :: Test
testList = TestList [testSquare, testColor, testPosition, testCastling]


testSquare :: Test
testSquare = TestCase $ assertEqual "square" (Just  $ Square 'e' 4 ) (decode "e4")

testColor :: Test
testColor = TestCase $ do
  assertEqual "white" (Just White) (decode "w")
  assertEqual "black" (Just Black) (decode "b")

testCastling :: Test
testCastling = TestList [testNoCastling]

testNoCastling :: Test
testNoCastling = TestLabel "parsing '-' as castling rights" $
               TestCase (assertBool "empty" $ Set.null $ fromJust $ decodeCastlingRights "-")

testPosition :: Test
testPosition = TestLabel (unwords ["decoding of ", sampleFen]) $ TestCase $ do
                 let p = fromJust $ decode sampleFen
                 assertColorAt White (Square 'c' 6) p
                 assertColorAt White (Square 'c' 7) p
                 assertColorAt Black (Square 'f' 3) p
                 assertColorAt Black (Square 'e' 4) p
                 assertColorAt Black (Square 'e' 5) p
                 assertColorAt Black (Square 'f' 6) p

assertColorAt :: Color -> Square -> Position -> Assertion
assertColorAt clr sq p = do
  let msg = unwords [show clr, "at", show sq]
  let clr' = colorAt p sq
  case clr' of
    Nothing -> assertFailure $ unwords ["No piece at", show sq, ". Expected", msg]
    Just c -> assertEqual msg clr c

sampleFen = "8/2K5/2Q2p2/4b3/4n3/5k2/8/8 w - - 7 53"
