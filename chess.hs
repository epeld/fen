module Chess where
import Control.Applicative
import Control.Monad
import Data.Maybe
import Square
import Game
import Piece
import MonadOps

--TODO REMEMBER PROMOTIONS!

{-
makeMove g s d = 
    let b = updateBoard s d g
        p = updateProperties s d g
     in 
       Game g s d

isValidMove g s d =
    let g' = makeMove g s d
     in
        isValidPosition g' && isReachable s d g
-}


isReachable s d g@(Game b _) = 
    case b !!! s of
        Nothing -> error "isReachable evaluated with Nothing"
        Just p  -> isReachable' s d (pieceType p) g
        
isReachable' s d Pawn g@(Game _ p) =
    let c   = whoseMove p
     in
        if isTakableByPawn d g then
            d `elem` pawnTakables s g else
            leadsTo d (pawnMovables s g) g

isReachable' s d (Officer t) g =
    leadsTo d (officerMovables s t) g

officerMovables s Bishop =
    sequence' s <$> [up 1 >=> right 1, up 1 >=> left 1,
                      down 1 >=> right 1, down 1 >=> left 1]

officerMovables s Rook = sequence' s <$> [up 1, left 1, down 1, right 1]
officerMovables s Queen = concat $ officerMovables s <$> [Rook, Bishop]
officerMovables s King = map (take 1) (officerMovables s Queen)
--officerMovables s Knight = fromSquare s <$> [up 1 >=> left 2

isTakable d (Game b p) =
    let pc = b !!! d
        c  = whoseMove p
     in
        maybe False (not . isOfColor c) pc

isTakableByPawn d g@(Game b p) =
    isTakable d g || isNothing (b !!! d) && isPassantSquare d g

relUp (Game _ p) =
    let c = whoseMove p
     in
        Square.relUp 1 c

pawnTakables s g@(Game _ p) =
    let up1 = Chess.relUp g
        left1 = left 1
        right1 = right 1
     in
        fromSquare s <$> [up1 >=> left1, up1 >=> right1]

pawnMovables s g@(Game _ p) = 
    let up1 = Chess.relUp g 
     in 
        fromSquare s <$> [up1, up1 >=> up1]

leadsTo d sq (Game b _) = 
    let nothingAt = isNothing . (b !!!)
        isntDest = (d /=)
        squares = takeWhile validSquare sq
     in
        case dropWhile (nothingAt `mAnd` isntDest ) squares of
            x : _ -> x == d
            _ -> False

