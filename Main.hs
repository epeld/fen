module Main where
import qualified System.Environment
import Control.Monad.Reader (runReader)
import Control.Monad (mapM)
import Text.ParserCombinators.Parsec (runParser)
import Data.Maybe (fromJust)
import qualified Chess
import qualified Types as Chess
import qualified PgnParse
import qualified Fen
import qualified Pgn
import Pgn (Error(..))

main :: IO ()
main = do
  printGreeting
  run
  printGoodbye

run :: IO ()
run = do
  args <- System.Environment.getArgs
  case args of
    [] -> printUsage
    moves -> runMoves moves


runMoves :: [String] -> IO ()
runMoves mvs = do
  case mapM (runParser PgnParse.move () "args") mvs of
    Left err -> putStrLn $ show err
    Right moves -> moveAll startPos moves

moveAll :: Chess.Position -> [Pgn.Move] -> IO ()
moveAll pos [] = return ()
moveAll pos (mv:mvs) =
  case runReader (Pgn.move mv) pos of
    Right pos' -> putPosLn pos' >> moveAll pos' mvs
    Left err -> putErrLn pos err

putPosLn :: Chess.Position -> IO ()
putPosLn = putStrLn. Fen.encode

putErrLn :: Chess.Position -> Pgn.Error -> IO ()
putErrLn pos err = putStrLn ("Error: " ++ readable pos err)

readable _ CaptureNotPossible = "Can't capture"
readable _ CaptureRequired = "Can't move. Square already occupied."
readable _ NoCandidate = "Can't find piece to move"
readable pos (AmbigousMove mvs) =
  "Ambiguos Move. Try one of:" ++
  unwords (map show (runReader (mapM Pgn.encode mvs) pos))


{-
readable KingCapturable = "King can be captured"
readable NoPiece = "There is no piece to move"
readable NotInRange = "Not in range"
readable MissingKing = "Illegal position. Missing king"
readable IllegalPromotion = "Illegal promotion"
readable PromotionRequired = "Must promote"
-}

printUsage :: IO ()
printUsage = putStrLn "Usage: fen e4 e5 Nc3 etc"

printGreeting :: IO ()
printGreeting = putStrLn "Hello."

printGoodbye :: IO ()
printGoodbye = putStrLn "Bye."

startPos :: Chess.Position
startPos =
  fromJust $
  Fen.decode "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"
