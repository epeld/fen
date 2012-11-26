module Chess where
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Reader
import Data.Maybe
import Square
import Game
import Piece

color :: GameReader Color
color = asks $ whoseMove . props
color' = invert <$> color

data MoveType = Moves | Takes deriving (Eq, Show)


manyRelBelow s c = squareSeriesV s (down1 c) 
manyRelAbove s c = squareSeriesV s (up1 c) 

diagonals s = let dirs = [(a,b) | a <- [1,-1],b <- [1,-1]]
               in map (squareSeries s) dirs

straights s = map (squareSeries s) [(1,0), (-1,0), (0,-1), (0,1)]

knights :: Square -> [Square]
knights s = let offs = liftM2 (,) [2,-2] [1,-1] ++ liftM2 (,) [1,-1] [2,-2]
                extractValidOnes = map fromJust . filter isJust
             in extractValidOnes $ offset s <$> offs

squares t m s c =
    case t of
        Pawn -> if m == Takes 
            then let d = down1 c
                  in return $ offsets s [(1,d), (-1,d)]
            else return $ take 2 $ manyRelBelow s c
        Officer t' -> officerSquares t' s

officerSquares t s =
    case t of
        Bishop -> diagonals s
        Queen  -> officerSquares Bishop s ++ officerSquares Rook s
        King   -> take 1 <$> officerSquares Queen s
        Rook   -> straights s
        Knight -> return <$> knights s

firstNonEmpties :: Game -> [[Square]] -> [Square]
firstNonEmpties g s =
    let firstNonEmpty = take 1 . dropWhile (squareIsEmpty g)
     in concat $ map firstNonEmpty s

matchPiece mp p = mp == Just p

candidates g t m s = do
    let c = whoseMove . props $ g
        hasCand s = pieceAt g s `matchPiece` Piece t c
     in return $ filter hasCand $ firstNonEmpties g (squares t m s c)

data MoveError = NothingToCapture | SquareOccupied | SameColorCapture

-- do pawn move specified the natural way (without pgn)
doNaturalPawnMove src m s promo g =
    do  g' <- possibleEnPassant m s g
        let b' = board g'
        let b'' = movePiece src s b'
        let g'' = Game b'' (propsAfter mv g)
        possiblePromotion s promo g''

possiblePromotion s promo g = 
    if isLastRank $ file s 
        then promote s promo g
        else return g

possibleEnPassant m s g =
    if m == Takes && enPassant g == Just s
        then case pieceAt s g of
            Nothing -> removeEP s g
            _ -> fail $ 
                "Invalid chess position; en passant square not empty"
        else return g

removeEP s g =
    let downDir = down1 . whoseMove . props $ g
        b = board g
        p = props g
        remove s' = Game (removePieceAt s' b) p
     in remove <$> voffset s downDir

promote s promo g =
     case promo of
        Just pr -> return $ put g s pr
        Nothing -> fail "Nothing to promote to"

propsAfter src mv g =
    let 
        c = invert $ whoseMove $ props g
        r = rightsAfter src mv g
        e = passantSquareAfter src mv g
        h = halfMoveAfter src mv g
        m = moveNumber p + 1
     in GameProperties $ c r e h m

passantSquareAfter src mv g = 
    let c = whoseMove $ props g
     in case mv of
        PawnMove _ Moves d Nothing -> 
            if isRankNr 4 c (rank d) 
                then Just $ voffset d (down1 c)
                else Nothing
        _ -> Nothing

halfMoveAfter src mv g =
    let h = halfMoveNumber $ props g
    in case mv of
        PawnMove _ _ _ _ -> 0
        OfficerMove _ _ takes _ -> 0
        _ -> h + 1

--TODO
rightsAfter s mv g =
    let r = castlingRights $ props g
     in case s of
        Sqare (file 'e') (rank 1) -> r // "KQ" 
        Sqare (file 'e') (rank 8) -> r // "kq" 
        Sqare (file 'a') (rank 1) -> r // "Q" 
        Sqare (file 'a') (rank 8) -> r // "q" 
        Sqare (file 'h') (rank 1) -> r // "K" 
        Sqare (file 'h') (rank 8) -> r // "k" 
