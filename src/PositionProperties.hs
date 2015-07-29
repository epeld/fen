{-# LANGUAGE TemplateHaskell #-}
module PositionProperties where
import Control.Lens

import qualified Data.Set as Set
import Square
import Piece
import Castling

data Properties = Properties {
    _turn :: Color,
    _passant :: Maybe Square,
    _fullMoveCount :: Int,
    _halfMoveCount :: Int,
    _castlingRights :: Set.Set CastlingRight }
    deriving (Show, Eq)

makeLenses ''Properties
