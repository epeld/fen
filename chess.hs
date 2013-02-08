module Chess where
import Control.Applicative
import Control.Monad
import Control.Arrow hiding (left,right)
import Data.Maybe
import Square
import Game
import Piece
import MonadOps

--TODO REMEMBER PROMOTIONS!

cantReach d s g@(Game b p) =
    let pc = b !!! s
        ss = squareToString s
        dd = squareToString d
     in
        case pc of
            Nothing -> fail $ "There is no piece at " ++ ss ++ "!"
            Just (Piece t _) -> fail $ (pieceTypeToString t) ++ " at " ++ ss ++
                " can't reach " ++ dd ++ "!"

gameAfterMove d s g =
    let b' = board g
        p' = properties g
     in
        return $ Game b' p'

makeMove d s g = 
    if isReachable s d g then
        gameAfterMove d s g else
        cantReach d s g
{-
isValidMove g s d =
    let g' = makeMove g s d
     in
        isValidPosition g' && isReachable s d g
-}

pieceAt s b = case b !!! s of
    Nothing -> error "Nothing at " ++ squareToString s
    Just p  -> p

-- TODO check if piece at d!
isReachable s d g@(Game b p) = 
    let t = pieceType p in isReachable' s d t g
        
isReachable' s d Pawn g@(Game _ p) =
    let c = whoseMove p
     in
        if isTakableByPawn d g then
            d `elem` pawnTakables s g else
            leadsTo d (pawnMovables s g) g

isReachable' s d (Officer t) g =
    let reaches sq = leadsTo d sq g
        sameColors = maybe False (whoseMove p ==) (color $ b !!! d)
     in
        if sameColors then False else
            or (reaches <$> officerMovables s t)

officerMovables :: Square -> OfficerType -> [[Square]]
officerMovables s Bishop =
    let upright = up 1 >=> right 1
        upleft  = up 1 >=> left 1
        downright = down 1 >=> right 1
        downleft  = down 1 >=> left 1
     in
        sequence' s <$> [upright, upleft, downright, downleft]

officerMovables s Rook   = sequence' s <$> [up 1, left 1, down 1, right 1]
officerMovables s Queen  = concat $ officerMovables s <$> [Rook, Bishop]
officerMovables s King   = take 1 <$> officerMovables s Queen
officerMovables s Knight = return <$> fromSquare s <$> knightJumps

knightJumps = 
    let long = [up, right] <*> [2,-2]
        short = [right, up] <*> [1,-1]
     in
        zipWith (>=>) long short

{-
knightJumps =
    let steps = (,) <$> [2,-2] <*> [1,-1]
        longShort = up *** right
        shortLong = right *** up 
     in
        concat $ [longShort, shortLong] <*> steps
-}

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

leadsTo :: Square -> [Square] -> Game -> Bool
leadsTo d sq (Game b _) = 
    let nothingAt = isNothing . (b !!!)
        isntDest = (d /=)
        squares = takeWhile validSquare sq
     in
        case dropWhile (nothingAt `mAnd` isntDest ) squares of
            x : _ -> x == d
            _ -> False

