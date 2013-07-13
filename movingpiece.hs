module MovingPiece (MovingPiece(..), movingPiece, piece, 
                    pieceType, color, officerType, isPawn,
                    friendlies, enemies, friendly, enemy)where
import Control.Monad.Error (throwError)
import Control.Applicative
import Data.Either (rights, lefts)
import Data.Maybe (fromJust, isNothing)

import Square (Square)
import qualified Piece 
import Color (Color)
import Position hiding (friendly, enemy)
import ErrorMonad 
import MoveType (MoveType)
import Piece (PieceType(Pawn))

data MovingPiece = PieceFromPosition { position :: Position, square::Square }
                                       deriving (Show, Eq)

enemies p = shouldntFail $ movingPiece p <$> enemySquares p
friendlies p = shouldntFail $ movingPiece p <$> friendlySquares p

friendly p s = movingPiece' p s (friendlyColor p)
enemy p s = movingPiece' p s (enemyColor p)

movingPiece' p s c = do
    mp <- movingPiece p s
    if color mp == c
        then return mp
        else throwError ColorsMismatch

movingPiece :: Position -> Square -> ErrorMonad MovingPiece
movingPiece p s = maybe errorNoPiece (return mp `const`) (readSquare p s)
    where errorNoPiece = throwError NoPieceToMove
          mp = PieceFromPosition p s


piece mp = fromJust $ position mp `readSquare` MovingPiece.square mp
color = Piece.color. piece
pieceType = Piece.pieceType. piece
officerType = Piece.officerType. pieceType

isPawn :: MovingPiece -> Bool
isPawn mp = pieceType mp == Pawn

shouldntFail mps = case lefts mps of
    [] -> rights mps
    x -> error $ show $ head mps
