module MoveVerification where
import Control.Lens
import Control.Applicative
import Control.Monad.Trans.Except

import Move
import PartialMove
import FullMove
import Piece
import Position
import MoveType

data Error = NoCandidate | Ambiguous [FullMove] | InvalidMoveType MoveType deriving (Show, Eq)


disambiguate :: [FullMove] -> Except [Error] FullMove
disambiguate [] = throwE [NoCandidate]
disambiguate [mv] = return mv
disambiguate mvs = throwE [Ambiguous mvs]


verifyMoveType :: Position -> PartialMove -> Except [Error] ()
verifyMoveType p (Partial mv) = 

    verify (mv ^. moveType)

    where

    verify Captures = verifyStandardCapture <|> verifyPassantCapture 
    verify Moves = verifyNoCapture 
        

    verifyStandardCapture =
        if p ^. board . at (mv ^. destination) ^? _Just . color == Just (p ^. turn . to otherColor)
        then pass
        else throwE (InvalidMoveType Captures : [])


    verifyPassantCapture =
        if p ^. board . at (mv ^. destination) == Nothing &&
           p ^. passant == Just (mv ^. destination) &&
           mv ^. Move.moveType == Captures &&
           mv ^. Move.pieceType == Pawn
        then pass
        else throwE (InvalidMoveType Captures : [])


    verifyNoCapture =
        if p ^. board . at (mv ^. destination) == Nothing
        then pass
        else throwE (InvalidMoveType Moves : [])


    pass = return ()
