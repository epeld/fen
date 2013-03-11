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

castlingAssociations = 
    let square f r = Square (File f) (Rank r)
     in [
        (square 'e' 1, whitesRights),
        (square 'h' 1, whitesRight' Kingside),
        (square 'a' 1, whitesRight' Queenside),
        (square 'e' 8, blacksRights),
        (square 'h' 8, blacksRight' Kingside),
        (square 'a' 8, blacksRight' Queenside)
        ]
                
colorAfterMove _ _ (Game _ p) = otherColor (whoseMove p)
rightsAfterMove d s (Game _ p) =
    let oldRights = castlingRights p
        associations  = castlingAssociations
        rightsToRemove s = maybe [] id (lookup s associations)
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

canMove d s g promo = isLegal (gameAfterMove d s g promo)

isFriendlyColor (Game _ p) = (whoseMove p ==)
isFriendlyPiece :: Game -> Piece -> Bool
isFriendlyPiece g = isFriendlyColor g . color
isFriendlyPieceAt :: Square -> Game -> Bool
isFriendlyPieceAt s g@(Game b _) = 
    maybe False (isFriendlyPiece g) (b !!! s)

findFriendlies g@(Game b p) = 
    let isFriendly :: Maybe Piece -> Bool
        isFriendly = maybe False (isFriendlyPiece g)
     in
        map toEnum (findIndices isFriendly b)

isLegal :: Game -> Bool
isLegal g = Nothing /= enemyKingSquare g && kingIsSafe g

isPiece p = (== p)

enemyKingSquare :: Game -> Maybe Square
enemyKingSquare g@(Game b p) =
    let rightPiece :: Maybe Piece -> Bool
        rightPiece =
            maybe False (isPiece (Piece (Officer King) (whoIsNotMoving p)))
     in toEnum <$> findIndex rightPiece b

takableFromSquare :: Square -> Game -> [Square]
takableFromSquare s g = 
    let moveOK = flip canMoveTo g
        takeOK = flip canTake g
        takables = pieceTakables s g
     in 
        filter takeOK <$> concat $ take 1 . dropWhile moveOK <$> takables

pieceTakables :: Square -> Game -> [[Square]]
pieceTakables s g@(Game b _) =
    case b !!! s of
        Nothing -> error $ nothingAt s
        Just p -> pieceTakables' p s g 

pieceTakables' :: Piece -> Square -> Game -> [[Square]]
pieceTakables' p s g@(Game b _) =
    case p of
        Piece Pawn _ -> pawnTakables s g
        Piece (Officer o) _ -> officerMovables s o

kingIsSafe g@(Game b p) = 
    let threatens d s = d `elem` takableFromSquare s g
        kingSquare = fromJust $ enemyKingSquare g
        friendlies = findFriendlies g
     in
        threatens kingSquare `any` friendlies

gameAfterMove d s g@(Game b _) promo =
    let b' = boardAfterMove d s g promo 
        p' = propertiesAfterMove d s g
     in
        Game b' p'

coloredPieces (Game b p) c =
    let isRightColoredPiece (Piece t c') = c == c'
     in
        findIndices (maybe False isRightColoredPiece) b

nothingAt :: Square -> String
nothingAt s = "Nothing at " ++ squareToString s

pieceAt s b = case b !!! s of
    Nothing -> error $ nothingAt s 
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

canTake :: Square -> Game -> Bool
canTake d (Game b p) =
    let 
        hasOppositeColor = (/= whoseMove p) . color
     in
        maybe False hasOppositeColor (b !!! d)

canTakeEP d (Game b p) = 
    case enPassantSquare p of
        Nothing -> False
        Just s  -> Nothing == b !!! d && d == s

canMoveTo :: Square -> Game -> Bool
canMoveTo d (Game b _) =
    Nothing == b !!! d

friendlyAt d = 
    concat [
        "Can't move to ",
        squareToString d,
        ". Square occupied by friendly piece!"
    ]

relUp (Game _ p) = Square.relUp 1 (whoseMove p)

pawnTakables s g@(Game _ p) =
    let up1 = Chess.relUp g
        left1 = left 1
        right1 = right 1
     in
        return . fromSquare s <$> [up1 >=> left1, up1 >=> right1]

pawnMovables s g@(Game _ p) = 
    let up1 = Chess.relUp g 
     in 
        fromSquare s <$> [up1, up1 >=> up1]
