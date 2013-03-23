module Move (Move, move) where

import Square
import Piece
import Internals
import Position

import Data.Maybe
import Control.Monad
import Control.Monad.Error 
import Control.Applicative

data Reason = LastRankPromote | NoPieceToMove | NoPromotion | ColorsMismatch

instance Error Reason where
noMsg = error "noMsg called"
strMsg s = "strMsg called"

type ErrorMonad = Either Reason

getPiece mp = fromJust $ getPosition mp `readSquare` getSquare mp
onPiece f = f . getPiece
whosePiece = onPiece color
whichPiece = onPiece pieceType

move :: LegalPosition -> Square -> Square -> Maybe Promotion -> ErrorMonad Move
move p s d pr = do
    mp <- movingPiece p s
    move' mp d pr

move' :: MovingPiece -> Square -> Maybe Promotion -> ErrorMonad Move
move' mp d pr = do
    verifyPromotion (rank d) (whosePiece mp) (whichPiece mp) pr
    return (Move mp d pr)

verifyPromotion 8 White Pawn Nothing = throwError LastRankPromote
verifyPromotion 1 Black Pawn Nothing = throwError LastRankPromote
verifyPromotion _ _ _ (Just _) = throwError NoPromotion

verifyHasColor :: Color -> Piece -> ErrorMonad ()
verifyHasColor c p = verifyColorsMatch c (color p)
verifyColorsMatch White White = return ()
verifyColorsMatch Black Black = return ()
verifyColorsMatch _ _ = throwError ColorsMismatch

movingPiece :: LegalPosition -> Square -> ErrorMonad MovingPiece
movingPiece p s = do
    let c = whoseTurn p
    let x = p `readSquare` s
    maybe (throwError NoPieceToMove) (verifyHasColor c) x
    return (PieceFromPosition p s)

{-
--classifyMove :: Move -> Maybe ClassifiedMove
--range :: MovingPiece -> PieceRange
-}
