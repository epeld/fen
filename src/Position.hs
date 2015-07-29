{-# LANGUAGE TemplateHaskell #-}
module Position where
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Lens

import MoveType
import Piece
import Square
import Castling
import PositionProperties as Properties


type PieceMap = Map.Map Square Piece


data Position = Position {
    _board :: PieceMap,
    _properties :: Properties
    } deriving (Show)
makeLenses ''Position


turn :: Simple Lens Position Color
turn = properties . Properties.turn


passant :: Simple Lens Position (Maybe Square)
passant = properties . Properties.passant


fullMoveCount :: Simple Lens Position Int
fullMoveCount = properties . Properties.fullMoveCount


halfMoveCount :: Simple Lens Position Int
halfMoveCount = properties . Properties.halfMoveCount


castlingRights :: Simple Lens Position (Set.Set CastlingRight)
castlingRights = properties . Properties.castlingRights


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
friendlyColor = Position.turn

enemyColor :: Getter Position Color
enemyColor = Position.turn . to otherColor
