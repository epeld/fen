{-# LANGUAGE TemplateHaskell #-}
module Position where
import Data.Map as Map
import qualified Data.Set as Set
import Control.Lens

import MoveType
import Piece
import Square
import Castling


type PieceMap = Map Square Piece


data Position = Position {
    _board :: PieceMap,
    _turn :: Color,
    _passant :: Maybe Square,
    _fullMoveCount :: Int,
    _halfMoveCount :: Int,
    _castlingRights :: Set.Set CastlingRight
    } deriving (Show)
makeLenses ''Position


movePiece :: Square -> Square -> Position -> Position
movePiece src dst p = p & board %~ movePiece'
    where
    movePiece' b =
        b & at src .~ Nothing 
          & at dst .~ (b ^. at dst)


filterPieces :: Position -> Piece -> PieceMap
filterPieces p pc = Map.filter (== pc) (p ^. board)


lastRank :: Color -> Int
lastRank White = 1
lastRank Black = 8


friendlyColor :: Simple Lens Position Color
friendlyColor = turn

enemyColor :: Getter Position Color
enemyColor = turn . to otherColor
