module Position where
import Prelude ()
import Data.Bool
import Data.Eq
import Data.Maybe
import Data.Map
import Data.Int
import Data.Function

import Text.Show
import Control.Monad.Reader
import qualified Data.Set as Set

import MoveType
import Piece
import Square
import Castling


type Board = Map Square Piece


data Position = Position {
    board :: Board,
    turn :: Color,
    passant :: Maybe Square,
    fullMoveCount :: Int,
    halfMoveCount :: Int,
    castlingRights :: Set.Set CastlingRight
    } deriving (Show)

hasPiece :: Position -> Piece -> Square -> Bool
hasPiece pos pc sq = pieceAt pos sq == Just pc

pieceAt :: Position -> Square -> Maybe Piece
pieceAt pos sq = lookup sq (board pos)

movePiece :: Square -> Square -> Position -> Position
movePiece src dst p = p { board = movePiece' src dst (board p) }
    where
    movePiece' src dst b = let pc = lookup src b
                            in update (const pc) dst (delete src b)

lastRank :: Color -> Int
lastRank White = 1
lastRank Black = 8
