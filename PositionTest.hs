module PositionTest where
import Test.HUnit.Base
import Test.HUnit.Text

import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Piece
import qualified Color
import Square (Square(Square), square)
import Position

run :: IO Counts
run = runTestTT tests

tests :: Test
tests = TestLabel "Position Tests" testList

testList :: Test
testList = TestList [testLastRank,
                     testNextTurn,
                     testEnemyColor,
                     testLookup,
                     testIsPassant,
                     testColorAt]


testLastRank :: Test
testLastRank = TestCase $
               assertEqual "Black last rank is 1" 1 (lastRank dummyPosition)

testNextTurn :: Test
testNextTurn = TestCase $
               assertEqual "Next turn is white" Color.White
               (turn $ nextTurn dummyPosition)


testEnemyColor :: Test
testEnemyColor = TestCase $
                 assertEqual "Black's enemy is white" Color.White
                 (enemyColor dummyPosition)

testLookup :: Test
testLookup = TestList [testLookupSuccess, testLookupFailure]

testLookupSuccess :: Test
testLookupSuccess = TestCase $
                    assertEqual "Pawn at e4"
                    (Just $ Piece.Piece Piece.Pawn Color.White)
                    (Position.lookup e4 dummyPosition)

testLookupFailure :: Test
testLookupFailure = TestCase $
                    assertEqual "Nothing at e3"
                    Nothing
                    (Position.lookup e3 dummyPosition)


testIsEmpty :: Test
testIsEmpty = TestCase $ assertBool "e3 is empty" (isEmpty dummyPosition e3)

testIsPassant :: Test
testIsPassant = TestList [testIsPassantFalse, testIsPassantTrue]

testIsPassantFalse :: Test
testIsPassantFalse = TestCase $ assertBool "e4 isn't passant"
                     (not $ isPassant dummyPosition e4)

testIsPassantTrue :: Test
testIsPassantTrue = TestCase $ assertBool "e3 is passant" (isPassant dummyPosition e3)


testColorAt :: Test
testColorAt = TestList [testColorOfPiece, testColorOfEmpty]

testColorOfPiece :: Test
testColorOfPiece = TestCase $
                   assertEqual "piece on e4 is white" (Just Color.White)
                   (colorAt dummyPosition e4)

testColorOfEmpty :: Test
testColorOfEmpty = TestCase $
                   assertEqual "no color at e3" Nothing (colorAt dummyPosition e3)

e3 = Square 'e' 3
e4 = Square 'e' 4

dummyPosition = Position {
  board = Map.singleton e4 (Piece.Piece Piece.Pawn Color.White),
  passant = Just e3,
  halfMoveNr = 1,
  fullMoveNr = 12,
  turn = Color.Black,
  castling = Set.empty }
