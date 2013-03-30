module Piece (
    PieceType(..),
    OfficerType(..),
    Piece(..),
    charToOfficerType,
    pieceTypeToString,
    verifyHasColor
    )where

import Data.Char
import Control.Monad.Error

import ErrorMonad (
    ErrorMonad,
    Reason(ColorsMismatch),
    )
import Color (
    Color(..),
    )

-- Differentiate pawns from officers
data PieceType = Pawn | Officer OfficerType deriving (Show, Eq)
data OfficerType = Bishop | Knight | Rook | Queen | King deriving (Show, Eq)
data Piece = Piece {
    pieceType :: PieceType,
    color :: Color
    } deriving (Show, Eq)

charToOfficerType c = lowerCharToOfficerType (toLower c)
lowerCharToOfficerType 'r' = Rook
lowerCharToOfficerType 'b' = Bishop
lowerCharToOfficerType 'k' = King
lowerCharToOfficerType 'q' = Queen
lowerCharToOfficerType 'n' = Knight

pieceTypeToString Pawn = "Pawn"
pieceTypeToString (Officer t) = show t

verifyHasColor :: Color -> Piece -> ErrorMonad ()
verifyHasColor c p = verifyColorsMatch c (color p)
verifyColorsMatch White White = return ()
verifyColorsMatch Black Black = return ()
verifyColorsMatch _ _ = throwError ColorsMismatch

