module MovingPiece (MovingPiece(..), movingPiece, piece, 
                    pieceType, color, officerType, isPawn,
                    friendlies, enemies, )where
import Control.Monad.Error (throwError)
import Control.Applicative
import Data.Either (rights, lefts)
import Data.Maybe (fromJust)

import Square (Square)
import qualified Piece 
import Color (Color)
import Position 
import ErrorMonad 
import MoveType (MoveType)
import Piece (PieceType(Pawn))

data MovingPiece = PieceFromPosition { position :: Position, square::Square }
                                       deriving (Show, Eq)

enemies p = shouldntFail $ movingPiece p <$> enemySquares p
friendlies p = shouldntFail $ movingPiece p <$> friendlySquares p

movingPiece :: Position -> Square -> ErrorMonad MovingPiece
movingPiece p s = do
    let c = whoseTurn p
    let x = p `readSquare` s
    maybe (throwError NoPieceToMove) (Piece.verifyHasColor c) x
    return (PieceFromPosition p s)

piece mp = fromJust $ position mp `readSquare` MovingPiece.square mp
color = Piece.color. piece
pieceType = Piece.pieceType. piece
officerType = Piece.officerType. pieceType

isPawn :: MovingPiece -> Bool
isPawn mp = pieceType mp == Pawn

shouldntFail mps = case lefts mps of
    [] -> rights mps
    x -> error $ show $ head mps
