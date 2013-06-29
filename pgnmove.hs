{-#LANGUAGE NoMonomorphismRestriction #-}
module PGNMove where
import Control.Monad
import Control.Monad.Error 
import Data.Either

import Square 
import Piece
import MoveType
import Piece
import CastlingSide
import ErrorMonad
import Position
import qualified MovingPiece
import qualified PGNMoveEssentials
import qualified MoveLogic
import qualified ChessMove
import qualified Castles

data PGNMove = 
    PawnMove {
        essentials :: PGNMoveEssentials.Essentials,
        promotion :: Maybe OfficerType
    } |
    OfficerMove {
        officerType :: OfficerType,
        essentials :: PGNMoveEssentials.Essentials
    } |
    Castles Side deriving (Show, Eq)

destination = PGNMoveEssentials.destination. essentials
moveType = PGNMoveEssentials.moveType. essentials

translate :: PGNMove -> Position -> ErrorMonad ChessMove.ChessMove
translate (Castles s) p = liftM ChessMove.Castling $ Castles.castles p s
translate mv p = oneValid $ map apply candidates
    where
        candidates = filter (compatible mv) (MovingPiece.friendlies p)
        apply mp = liftM ChessMove.Standard $ MoveLogic.move mp mt d pr
        d = destination mv
        mt = moveType mv
        pr = case mv of
            PawnMove _ _ -> promotion mv
            _ -> Nothing

oneValid :: [ErrorMonad ChessMove.ChessMove] -> ErrorMonad ChessMove.ChessMove
oneValid mvs = case valids of
    [x] -> return x
    [] -> throwError $ NoCandidates reasons
    _ -> throwError $ TooManyCandidates squares
    where
        valids = rights mvs
        reasons = lefts mvs
        squares = map ChessMove.square valids

compatible mv mp = PGNMoveEssentials.compatible e mp && matchesPieceType mv mp
    where e = essentials mv

matchesPieceType (PawnMove _ _) mp = MovingPiece.pieceType mp == Pawn
matchesPieceType (OfficerMove t _) mp = MovingPiece.pieceType mp == Officer t
