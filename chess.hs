module Chess where
import Control.Applicative
import Control.Monad
import Control.Arrow hiding (left,right)
import Data.List
import Data.Maybe
import Square
import Game
import Piece
import MonadOps

cantReach d s g@(Game b p) =
    maybe (noPieceStr s) (pieceCantReach d) (b !!! s)

noPieceStr s = "No piece at " ++ squareToString s
pieceTypeCantReach d t =
    pieceTypeToString t ++ " can't reach" ++ squareToString d
pieceCantReach d (Piece t _) = 
    pieceTypeCantReach d t
                
colorAfterMove _ _ (Game _ p) = otherColor (whoseMove p)
rightsAfterMove d s (Game _ p) =
    let oldRights = castlingRights p
        square f r = Square (File f) (Rank r)
        associations  = [
                (square 'e' 1, whitesRights),
                (square 'h' 1, return $ whitesRight Kingside),
                (square 'a' 1, return $ whitesRight Queenside),
                (square 'e' 8, blacksRights),
                (square 'h' 8, return $ blacksRight Kingside),
                (square 'a' 8, return $ blacksRight Queenside)
                ]
        rightsToRemove s = case lookup s associations of
            Just x -> x
            Nothing -> []
     in
        (oldRights \\ rightsToRemove s) \\ rightsToRemove d

enPassantAfterMove d s g@(Game _ p) =
    let up1 = Chess.relUp g
        rank' = fromEnum . rank
     in
        if file d == file s && abs (rank' s - rank' d) == 2 then
            Just (fromSquare s up1) else
            Nothing

isPawn p = pieceType p == Pawn

halfMovesAfterMove d s g@(Game b p) =
    let isCapture = Nothing /= b !!! d
        isPawnMove = maybe False isPawn (b !!! s)
     in
        if isCapture || isPawnMove
        then
            0
        else
            halfMoveNumber p + 1

moveNumberAfterMove _ _ (Game _ p) = moveNumber p + 1

propertiesAfterMove d s g@(Game _ p) =
    GameProperties {
        whoseMove      = colorAfterMove d s g,
        castlingRights = rightsAfterMove d s g,
        enPassantSquare = enPassantAfterMove d s g,
        halfMoveNumber = halfMovesAfterMove d s g,
        moveNumber = moveNumberAfterMove d s g
        }

isPromotionMove d s (Game b p) =
    let isPawnMove = maybe False isPawn (b !!! s)
        c = whoseMove p
     in
        isPawnMove && rank d == lastRank c

boardAfterMove d s g@(Game b p) promo = 
    let sansPromo = move' s d b
        newPiece = Piece (Officer promo) (whoseMove p)
     in
        if isPromotionMove d s g
            then
                replace' d (Just newPiece) sansPromo
            else
                sansPromo

gameAfterMove d s g@(Game b _) promo =
    let b' = boardAfterMove d s g promo 
        p' = propertiesAfterMove d s g
     in
        return $ Game b' p'

coloredPieces (Game b p) c =
    let isRightColoredPiece (Piece t c') = c == c'
     in
        findIndices (maybe False isRightColoredPiece) b

pieceAt s b = case b !!! s of
    Nothing -> error $ "Nothing at " ++ squareToString s
    Just p  -> p

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

isTakable d (Game b p) =
    let 
        hasOppositeColor = (/= whoseMove p) . color
     in
        maybe False hasOppositeColor (b !!! d)

isEPTakable d (Game b p) = 
    case enPassantSquare p of
        Nothing -> False
        Just s  -> Nothing == b !!! d && d == s

isMovable d (Game b _) =
    Nothing == b !!! d

friendlyAt d =
    "Can't move to " ++
    squareToString d ++ 
    ". Square occupied by friendly piece!"

relUp (Game _ p) = Square.relUp 1 (whoseMove p)

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
