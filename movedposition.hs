module MovedPosition (naivePositionAfter, kingIsSafe, squareIsThreatened) where
import Control.Monad.State (runState)
import Control.Monad (when, unless)
import Data.Maybe (fromJust)
import Data.Char (toLower)

import ChessMove
import qualified Castles
import qualified Move
import Board (move, remove,)
import Position 
import Color 
import MoveType 
import Square (up, down)
import PawnRange (pawnDirection,)

naivePositionAfter mv = Position
    (boardAfter mv)
    (whoseTurnAfter mv)
    (enPassantAfter mv)
    (castlingRightsAfter mv)
    (fullMovesAfter mv)
    (halfMovesAfter mv)

enPassantAfter (Right _) = Nothing
enPassantAfter (Left mv) = if isTwoStepPawnMove mv
    then Just $ passantSquare mv else Nothing

-- Turns since the last pawn advance or capture
halfMovesAfter (Right _) = 0
halfMovesAfter (Left mv) = if isPawnMove mv || moveType mv == Takes
    then 1 else oldValue + 1
    where oldValue = halfMoves. position $ mv

whoseTurnAfter = enemyColor. position

fullMovesAfter mv = case whoseTurnAfter mv of
    White -> oldValue + 1
    Black -> oldValue
    where oldValue = fullMoves. position $ mv

boardAfter mv = snd $ runState (makeMove mv) (board mv)
    where makeMove (Right mv) = do
              Board.move (kingSquare mv) (kingSourceSquare mv)
              Board.move (rookSourceSquare mv) (rookDestinationSquare mv)

          makeMove (Left mv) = do
              Board.move (square mv) (destination mv)
              when (isPassantMove mv) $ do
                  mpc <- removePassantPawn mv
                  checkIsEnemyPawn mpc mv
                  return ()

removePassantPawn mv = remove . passantSquare $ mv

passantSquare mv = fromJust $ backward $ destination mv
    where backward = pawnDirection $ invert $ whose mv


checkIsEnemyPawn (Just pc) mv = checkIsPawnColored pc (Move.enemyColor mv)

checkIsPawnColored pc c =
    unless (pc == coloredPawn) (error $ "Not a " ++ cs ++ " pawn")
    where coloredPawn = Piece Pawn c
          cs = toLower <$> show c

castlingRightsAfter (Right mv) = rightsBefore \\ friendlyRights
    where rightsBefore = castlingRights (position mv)
          friendlyRights = Castles <$> [Kingside, Queenside] <*> [whose mv]
castlingRightsAfter mv = [] -- TODO
