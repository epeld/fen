module MovedPosition (naivePositionAfter, squareIsThreatened) where
import Control.Monad.State (runState)
import Control.Monad (when, unless)
import Control.Applicative
import Data.Maybe (fromJust)
import Data.Char (toLower)
import Data.List ((\\))

import qualified ChessMove
import qualified Move
import qualified Castles
import CastlingSide
import CastlingRight
import Board (move, remove,)
import Position 
import Piece
import Color 
import MoveType 
import Square (up, down)
import PawnRange 
import Threats

naivePositionAfter mv = Position
    (boardAfter mv)
    (whoseTurnAfter mv)
    (enPassantAfter mv)
    (castlingRightsAfter mv)
    (fullMovesAfter mv)
    (halfMovesAfter mv)

enPassantAfter (Right _) = Nothing
enPassantAfter (Left mv) = if Move.isTwoStepPawnMove mv
    then Just $ passantSquare mv else Nothing

-- Turns since the last pawn advance or capture
halfMovesAfter (Right _) = 0
halfMovesAfter (Left mv) = if Move.isPawnMove mv || Move.moveType mv == Takes
    then 1 else oldValue + 1
    where oldValue = halfMoves. Move.position $ mv

whoseTurnAfter = enemyColor. ChessMove.position

fullMovesAfter mv = case ChessMove.whoseTurnAfter mv of
    White -> oldValue + 1
    Black -> oldValue
    where oldValue = fullMoves. ChessMove.position $ mv

boardAfter mv = snd $ runState (makeMove mv) (ChessMove.board mv)
    where makeMove (Right mv) = do
              let cr = Castles.expendedCastlingRight mv

              Board.move
                (Castles.kingSquare $ Castles.whose mv)
                (Castles.kingDestinationSquare cr)

              Board.move
                (Castles.rookSourceSquare cr)
                (Castles.rookDestinationSquare cr)

          makeMove (Left mv) = do
              Board.move (Move.square mv) (Move.destination mv)
              when (Move.isPassantMove mv) $ do
                  mpc <- removePassantPawn mv
                  checkIsEnemyPawn mpc mv
                  return ()

removePassantPawn mv = remove . passantSquare $ mv

passantSquare mv = fromJust $ backward $ Move.destination mv
    where backward = pawnDirection $ invert $ Move.whose mv


checkIsEnemyPawn (Just pc) mv = checkIsPawnColored pc (Move.enemyColor mv)

checkIsPawnColored pc c =
    unless (pc == coloredPawn) (error $ "Not a " ++ cs ++ " pawn")
    where coloredPawn = Piece Pawn c
          cs = toLower <$> show c

castlingRightsAfter (Right mv) = rightsBefore \\ friendlyRights
    where rightsBefore = castlingRights (Castles.position mv)
          friendlyRights = Castles <$> [Kingside, Queenside] <*> [Castles.whose mv]

castlingRightsAfter mv = [] -- TODO
