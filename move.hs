module Move(Move, move) where

import Square
import Piece
import Internals
import Position

import Data.Maybe
import Control.Monad
import Control.Applicative

getPiece mp = fromJust $ getPosition mp `readSquare` getSquare mp
onPiece f = f . getPiece
whosePiece = onPiece color
whichPiece = onPiece pieceType

move :: LegalPosition -> Square -> Square -> Maybe Promotion -> Maybe Move
move p s d pr = do
    mp <- movingPiece p s
    move' mp d pr

move' :: MovingPiece -> Square -> Maybe Promotion -> Maybe Move
move' mp d pr = do
    verifyPromotion (rank d) (whosePiece mp) (whichPiece mp) pr
    return (Move mp d pr)

verifyPromotion 8 White Pawn Nothing = raiseError LastRankPromote
verifyPromotion 1 Black Pawn Nothing = raiseError LastRankPromote
verifyPromotion _ _ _ (Just _) = raiseError NoPromotion

verifyHasColor c p = verifyColorsMatch c (color p)
verifyColorsMatch White White = return ()
verifyColorsMatch Black Black = return ()
verifyColorsMatch _ _ = raiseError ColorsMismatch

movingPiece :: LegalPosition -> Square -> Maybe MovingPiece
movingPiece p s = do
    let c = whoseTurn p
    x <- p `readSquare` s
    verifyHasColor c x
    return (PieceFromPosition p s)

--classifyMove :: Move -> Maybe ClassifiedMove
--range :: MovingPiece -> PieceRange
