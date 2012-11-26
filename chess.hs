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
matchColor c (Piece _ c2) = c == c2

candidates t m s g = do
    let c = whoseMove . props $ g
        hasCand s = pieceAt g s `matchPiece` Piece t c
     in return $ filter hasCand $ firstNonEmpties g (squares t m s c)

allCandidates m s g = 
    let types = pawn : officers
        officers = Officer <$> [Bishop, Knight, Rook, Queen, King]
     in \t -> candidates t m s g <$> types

data MoveError = 
    NothingToCapture |
    SquareOccupied |
    SameColorCapture |
    KingInDanger |
    NoKing deriving (Show, Eq)

-- do pawn move specified the natural way (without pgn)
doNaturalPawnMove src m s promo g =
    do  g' <- possibleEnPassant m s g
        let d = destination mv
        let b' = board g'
        let b'' = movePiece src s b'
        let g'' = Game b'' (propsAfter src d g)
        assertCanPawn m s g
        possiblePromotion s promo g'' >>= $
        assertKingIsSafe

doNaturalOfficerMove src m s g =
    do  let b' = movePiece src s (board g)
        let p' = propsAfter src (destination mv) g
        assertCanOfficer m s g
        assertKingIsSafe $ Game b' p'

doCastles s g =
    do  let b' = movePiece ks kd . movePiece rk rd $ (board g)
        let p' = propsAfterCastles g
        assertCanCastle s g
        assertKingIsSafe $ Game b' p'

assertCanOfficer Moves s g =
    case pieceAt s g of
        Nothing -> return g
        _ -> Just SquareOccupied

assertCanOfficer Takes s g =
    case pieceAt s g of
        Nothing -> fail NothingToCapture
        Just (Piece _ c) -> if c /= whoseMove (props g)
            then return g
            else fail SameColorCapture

findKing g =
    let p = props g
        c = whoseMove p
        king = Piece (Officer King) c
     in case find ((king ==) . snd) (assocs g) of
        Nothing -> fail NoKing
        Just (s,_) -> return s

assertKingIsSafe g =
    do  s <- findKing
        case allCandidates g Takes s of
            [] -> return g
            _  -> fail KingInDanger

assertCanPawn Moves s g = assertCanOfficer Moves s g

assertCanPawn Takes s g =
    let p = props g
    in case pieceAt s g of
        Nothing -> if s == enPassantSquare p
            then return g
            else fail NothingToCapture
        Just (Piece _ c) -> if c != whoseMove p
            then return g
            else fail $ SameColorCapture

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

propsAfter src dst g =
    let 
        c = invert $ whoseMove $ props g
        r = rightsAfter src dst g
        e = passantSquareAfter src dst g
        h = halfMoveAfter src dst g
        m = moveNumber p + 1
     in GameProperties $ c r e h m

passantSquareAfter src dst g = 
    let c = whoseMove $ props g
     in case pieceAt src g of
        Just (Piece Pawn _) ->
            if isRankNr 4 c (rank dst) 
                then Just $ voffset dst (down1 c)
                else Nothing
        Nothing -> error "passantSquareAfter chess.hs"
        _ -> Nothing

halfMoveAfter src dst g =
    let h = halfMoveNumber $ props g
        c = whoseMove $ props g
        b = board g
        isCapture = isJust $ pieceAt dst b -- TODO check for right color?
        isPawnMove = Just (Piece Pawn c) == pieceAt src b
     in if isCapture || isPawnMove
        then 0
        else h + 1

rightsAfter s d g =
    let r = castlingRights $ props g
        c = whoseMove $ props g
        dueToSquare s' = case s' of
            Sqare (file 'e') (rank 1) -> "KQ" 
            Sqare (file 'e') (rank 8) -> "kq" 
            Sqare (file 'a') (rank 1) -> "Q" 
            Sqare (file 'a') (rank 8) -> "q" 
            Sqare (file 'h') (rank 1) -> "K" 
            Sqare (file 'h') (rank 8) -> "k" 
       in r // (dueToSquare s ++ dueToSquare d)
