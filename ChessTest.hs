module ChessTest where
import Control.Monad.Reader (runReader)
import Control.Applicative ((<*>), (<$>))
import Test.HUnit.Base
import Test.HUnit.Text
import Data.Set (fromList)
import Data.Maybe (fromJust)
import Types as Chess
import qualified Fen
import Chess

predefined :: String -> Chess.Position
predefined = fromJust. Fen.decode

startFen :: String
startFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

withFen fen = (`runReader` predefined fen)


tests = allUtilTests

run = runTestTT tests


allUtilTests = TestList $ utilTests ++ utilMiniTests


utilTests = [lostCastlingTest, adjacentFilesTest]

utilMiniTests = map TestCase utilAssertions
utilAssertions = [assertEnemyColor, assertToggleWhite, assertToggleBlack]

assertEnemyColor = assertEqual "enemy color" (withFen startFen enemyColor) Chess.Black
assertToggleWhite = assertEqual "toggle white" Black (toggle White)
assertToggleBlack = assertEqual "toggle black" White (toggle Black)

e1 = Square 'e' 1
e4 = Square 'e' 4
e8 = Square 'e' 8

h1 = Square 'h' 1
h8 = Square 'h' 8

a1 = Square 'a' 1
a8 = Square 'a' 8

f8 = Square 'f' 8

adjacentFilesTest = TestCase $ do
  assertAdjacent e4 f8
  assertAdjacent f8 e4
  assertBool "e1 e4 not adjacent files" (not (adjacentFiles e1 e8))

assertAdjacent s1 s2 = do
  let msg = (Fen.encode s1) ++ " " ++ (Fen.encode s2) ++ " have adjacent files"
  assertBool msg (adjacentFiles s1 s2)

lostCastlingTest = TestList [whiteKingMoveLosesBothRightsTest,
                             whiteKingRookMoveLosesKingsideRightTest,
                             whiteQueenRookMoveLosesQueensideRightTest,
                             blackKingMoveLosesBothRightsTest,
                             blackKingRookMoveLosesKingsideRightTest,
                             blackQueenRookMoveLosesQueensideRightTest]

whiteKingMoveLosesBothRightsTest =
  testLoses e1 (Castling <$> [Queenside, Kingside] <*> [White])

blackKingMoveLosesBothRightsTest =
  testLoses e8 (Castling <$> [Queenside, Kingside] <*> [Black])

whiteKingRookMoveLosesKingsideRightTest =
    testLoses h1 (Castling <$> [Kingside] <*> [White])

whiteQueenRookMoveLosesQueensideRightTest =
    testLoses a1 (Castling <$> [Queenside] <*> [White])

blackKingRookMoveLosesKingsideRightTest =
    testLoses h8 (Castling <$> [Kingside] <*> [Black])

blackQueenRookMoveLosesQueensideRightTest =
    testLoses a8 (Castling <$> [Queenside] <*> [Black])

testLoses :: Square -> [CastlingRight] -> Test
testLoses sq rights = TestCase $ do
  let lost = lostCastling sq
  let msg = "moved from/to " ++ (Fen.encode sq)
  assertEqual msg (fromList rights) lost
