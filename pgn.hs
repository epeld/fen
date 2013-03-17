{-#LANGUAGE NoMonomorphismRestriction #-}
module Pgn where

import Square 
import Game
import Piece
import MonadOps
import Data.Char
import Data.List
import Text.Parsec
import Control.Monad
import Control.Applicative

data MoveType = Takes | Moves deriving (Show, Eq)

data PGNMoveEssentials = PGNMoveEssentials {
    hint :: (Maybe Hint),
    moveType :: MoveType,
    destination :: Square
    } deriving (Show, Eq)

data PGNMove = PawnMove {
    essentials :: PGNMoveEssentials,
    promotion :: (Maybe OfficerType)
    } |
    OfficerMove {
        officerType :: OfficerType,
        essentials :: PGNMoveEssentials
    } |
    Castles Side deriving (Show)

data Hint = FileHint File | RankHint Rank | SquareHint Square deriving (Show, Eq)

hintFromMove = hint . essentials

pieceTypeFromMove (PawnMove _ _) = Pawn
pieceTypeFromMove (OfficerMove t _) = Officer t
pieceTypeFromMove _ = error "Move has no piece type"

candidate mv g = 
    case take 1 (candidates mv g) of
        [x] -> Just x
        _ -> Nothing

candidates :: PGNMove -> Game -> [Square]
candidates (Castles _) g = error "Castling moves lack candidates"
candidates mv g = candidates' mv (hintFromMove mv) g

rightPiece mv p = Piece pt c
    where pt = pieceTypeFromMove mv
          c = whoseMove p

matchPiece p = (p==)
maybeMatchPiece p = maybe False (matchPiece p)

maybeMatchRightPiece mv p = maybeMatchPiece $ rightPiece mv p

pieceSquares mv (Game b p) = findIndices (maybeMatchRightPiece mv p) b
pieceIndices mv g = toEnum <$> pieceSquares mv g

candidates' :: PGNMove -> (Maybe Hint) -> Game -> [Square]
candidates' mv mh g = filter (matchMaybeHint mh) (pieceIndices mv g)

matchHint :: Hint -> Square -> Bool
matchHint (FileHint f) s = f == file s
matchHint (RankHint r) s = r == rank s
matchHint (SquareHint s) s2 = s == s2

matchMaybeHint :: Maybe Hint -> Square -> Bool
matchMaybeHint mh s = maybe True (flip matchHint s) mh
