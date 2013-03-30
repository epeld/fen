module Move (Move, move) where

import Square (
    Square, rank
    )
import Piece (
    color,
    Piece,
    PieceType(..), pieceType,
    OfficerType,
    )
import Color (
    Color(..)
    )
import ErrorMonad (
    ErrorMonad,
    Reason(NoPromotion, LastRankPromote),
    )
import Position (
    Position,
    readSquare,
    Promotion,
    )
import MovingPiece (
    MovingPiece, movingPiece,
    position, square
    )

import Data.Maybe (
    fromJust
    )
import Control.Monad.Error (
    throwError
    )

data Move = Move MovingPiece Square (Maybe Promotion)
data ClassifiedMove = Standard Move | Capturing Move

getPiece mp = fromJust $ position mp `readSquare` MovingPiece.square mp
onPiece f = f . getPiece
whosePiece = onPiece color
whichPiece = onPiece pieceType

move :: Position -> Square -> Square -> Maybe Promotion -> ErrorMonad Move
move p s d pr = do
    mp <- movingPiece p s
    move' mp d pr

move' :: MovingPiece -> Square -> Maybe Promotion -> ErrorMonad Move
move' mp d pr = do
    verifyPromotion (rank d) (whosePiece mp) (whichPiece mp) pr
    return (Move mp d pr)

verifyPromotion 8 White Pawn Nothing = throwError LastRankPromote
verifyPromotion 1 Black Pawn Nothing = throwError LastRankPromote
verifyPromotion _ _ _ (Just _) = throwError NoPromotion
verifyPromotion _ _ _ _ = return ()
