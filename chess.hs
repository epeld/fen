module Chess where
import Control.Applicative
import Control.Monad
import Data.Maybe
import Square
import Game
import Piece

--TODO REMEMBER PROMOTIONS!

makeMove g s d = 
    let b = updateBoard s d g
        p = updateProperties s d g
     in 
       Game g s d

isValidMove g s d =
    let g' = makeMove g s d
     in
        isValidPosition g' && isReachable s d g


isReachable s d g = 
    let t = pieceType $ b !! s
     in
        isReachable' s d t g

isReachable' s d Pawn g@(Game _ p) =
    let c   = whoseMove p
     in
        if isTakableByPawn d g then
            d `elem` pawnTakables s g else
            d `elem` pawnMovables s g

isReachable' s d (Officer t) g =
    False



isTakable d (Game b p) =
    let pc = b !! d
        c  = whoseMove p
     in
        maybe False (not . isOfColor $ c) pc

isTakableByPawn d g@(Game b p) =
    isTakable d g || isNothing (b !! d) && isPassantSquare d g

relUp (Game _ p) =
    let c = whoseMove p
     in
        Square.relUp 1 c

pawnTakables s g@(Game _ p) =
    let up1 = relUp g
        left1 = left 1
        right1 = right 1
     in
        fromSquare s <$> [up1 >=> left1, up1 >=> right1]

pawnMovables s g@(Game _ p) = 
    let up1 = relUp g 
     in 
        fromSquare <$> [up1, up1 >=> up1]
