module Chess where
import Prelude hiding (lookup)
import Control.Applicative ((<|>), (<$>), (<*>), Applicative)
import Control.Monad (liftM2, (>=>), (>>=), (=<<))
import Data.Map (insert, delete, keys, Map, lookup)
import Data.List (find)
import Data.Set (fromList, Set, difference)
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe, fromJust)

data Error = KingCapturable | 
             NoPiece |
             WrongColor |
             NotInRange |
             MissingKing |
             IllegalPromotion |
             PromotionRequired
             deriving (Show, Eq)

data Color = White | Black
             deriving (Show, Eq, Ord)

data Side = Queenside | Kingside
            deriving (Show, Eq, Ord)

data CastlingRight = Castling { side :: Side, clr :: Color }
                     deriving (Show, Eq, Ord)

data Piece = Piece { pieceType :: PieceType, color :: Color }
             deriving (Show, Eq)

data PieceType = Pawn |
                 Officer OfficerType
                 deriving (Show, Eq)

data OfficerType = Bishop | Rook | King | Queen | Knight
                   deriving (Show, Eq)

data Square = Square { file :: Char, rank :: Int }
              deriving (Show, Eq, Ord)

type Board = Map Square Piece

data Position = Position { board :: Board,
                           passant :: Maybe Square,
                           halfMoveNr :: Int,
                           fullMoveNr :: Int,
                           turn :: Color,
                           castling :: Set CastlingRight }
                           deriving (Show, Eq)

data Move = Move { source :: Square,
                   destination :: Square,
                   promotion :: Maybe PieceType }
            deriving (Show, Eq)


move :: Position -> Move -> Either Error Position
move pos mv = do
    p <- maybePromote mv <$> moveNaive pos mv 
    let errs = checkPromotion pos mv <|> checkLegal p
    maybeError errs p

maybePromote :: Move -> Position -> Position
maybePromote mv pos = 
    case promotion mv of
        Just pt -> pos { board = insert dest (Piece pt clr) b }
        Nothing -> pos
    where b = board pos
          clr = turn pos
          dest = destination mv

-- Naive move := move disregarding king safety and promotions
moveNaive :: Position -> Move -> Either Error Position
moveNaive pos mv = maybeError errs (performMove pos mv)
    where errs = checkSource pos mv <|> checkRange pos mv

checkRange :: Position -> Move -> Maybe Error
checkRange pos mv = 
    case pieceTypeAt pos src of
        Nothing -> Just NoPiece
        Just pt -> checkInRange pos dest (range pt pos src) 
    where dest = destination mv
          src = source mv
          range = if isCapture pos mv
                  then threats
                  else moves

checkPromotion pos mv =
    case promotion mv of
        Nothing -> if shouldPromote pos mv
                   then Just PromotionRequired
                   else Nothing

        Just _ -> if shouldPromote pos mv
                  then Nothing
                  else Just IllegalPromotion

shouldPromote pos mv = pawnAt pos (source mv) && onLastRank pos (destination mv)

checkLegal :: Position -> Maybe Error
checkLegal pos = checkKingSafe pos <|> checkPromotedPawns pos

checkPromotedPawns :: Position -> Maybe Error
checkPromotedPawns pos = if any (onLastRank pos) pawnSquares
                         then Just PromotionRequired
                         else Nothing
    where pawnSquares = filter (pawnAt pos) (friendlySquares pos)

onLastRank pos sq = rank sq == lastRank pos

checkKingSafe :: Position -> Maybe Error
checkKingSafe = maybe (Just MissingKing) <$> checkAttacked <*> kingSquare

kingSquare :: Position -> Maybe Square
kingSquare pos = findSquare (kingAt pos `fAnd` enemyAt pos) pos

checkAttacked :: Position -> Square -> Maybe Error
checkAttacked pos sq = if isAttacked pos sq
                       then Just KingCapturable
                       else Nothing

isAttacked :: Position -> Square -> Bool
isAttacked pos sq = any couldMove (friendlySquares pos)
    where couldMove :: Square -> Bool
          couldMove src = canMoveNaive pos (Move { source = src,
                                                   destination = sq,
                                                   promotion = Nothing })

friendlySquares :: Position -> [Square]
friendlySquares pos = filter (friendlyAt pos) (occupiedSquares pos)

                           
canMoveNaive :: Position -> Move -> Bool
canMoveNaive pos mv = isRight $ moveNaive pos mv

checkInRange :: Position -> Square -> [[Square]] -> Maybe Error
checkInRange pos dest range = if any (reaches pos dest) range
                              then Nothing 
                              else Just NotInRange

reaches :: Position -> Square -> [Square] -> Bool
reaches pos sq series = let sqs = takeWhile (isEmpty pos) series
                         in sq `elem` (take 8 series)

threats :: PieceType -> Position -> Square -> [[Square]]

threats Pawn pos source = let fw = ahead pos source
                              sqs = [ left =<< fw, right =<< fw ] 
                           in return <$> catMaybes sqs

threats pt pos source = moves pt pos source

moves :: PieceType -> Position -> Square -> [[Square]]
moves Pawn pos src = map (take 2) (applySteppers [up] src)

moves (Officer o) pos source = moves' o source

moves' Bishop s = applySteppers [ upLeft, upRight, downLeft, downRight ] s
moves' Rook s = applySteppers [ up, down, left, right ] s
moves' Queen s = moves' Bishop s ++ moves' Rook s
moves' King s = map (take 1) (moves' Queen s)
moves' Knight s = return <$> knightSquares s

knightSquares :: Square -> [Square]
knightSquares s = stepSteppers steppers s
    where makeStepper = foldl (>=>) return
          steppers = map makeStepper [ [up, up, left],
                                       [up, up, right],
                                       [up, right, right],
                                       [down, right, right],
                                       [down, down, right],
                                       [down, down, left],
                                       [down, left, left],
                                       [up, left, left] ]
          

type Stepper = Square -> Maybe Square

-- Make one step with each stepper
stepSteppers :: [Stepper] -> Square -> [Square]
stepSteppers steppers source = catMaybes $ map ($ source) steppers

-- Run a stepper until it doesn't produce squares anymore
applyStepper stepper s = catMaybes $ takeWhile isJust $ drop 1 $
                         iterate (>>= stepper) (Just s)

applySteppers :: [Stepper] -> Square -> [[Square]]
applySteppers steppers source = applyStepper <$> steppers <*> [source]
       
isCapture :: Position -> Move -> Bool
isCapture pos mv = enemyAt pos (destination mv) || isPassantCapture pos mv

isPassantCapture :: Position -> Move -> Bool
isPassantCapture pos mv = isJust (passantCapture pos mv)



enemyPawnAt :: Position -> Square -> Bool
enemyPawnAt pos dest = enemyAt pos dest && pawnAt pos dest
                                           

-- peformMove is the function that actually performs the work;
-- moves the piece from source to dest and updates the position
performMove  :: Position -> Move -> Position
performMove pos mv = pos { turn = calcTurn pos,
                           board = calcBoard pos mv,
                           fullMoveNr = calcFullMoveNr pos,
                           halfMoveNr = calcHalfMoveNr pos mv,
                           passant = calcPassant pos mv,
                           castling = calcCastling pos mv }


calcHalfMoveNr pos mv = if pawnAt pos (source mv) || isCapture pos mv
                        then 0
                        else 1 + halfMoveNr pos
calcPassant pos mv = 
    if pieceTypeAt pos src == Just Pawn &&
       Just dest == aheadN pos src 2 &&
       rank src == initialPawnRank
    then back pos src
    else Nothing
    where src = source mv
          dest = destination mv
          initialPawnRank = case turn pos of
              White -> 2
              Black -> 7

calcFullMoveNr :: Position -> Int
calcFullMoveNr pos = 1 + fullMoveNr pos

calcTurn :: Position -> Color
calcTurn = toggle. turn

calcHalfMove :: Position -> Square -> Square -> Int
calcHalfMove pos source dest = if pawnAt pos source || enemyAt pos dest
                               then 0
                               else halfMoveNr pos + 1

calcCastling :: Position -> Move -> Set CastlingRight
calcCastling pos mv = 
    let src = source mv
        dest = destination mv
        lost = Data.Set.fromList $ mapMaybe lostCastling [src, dest]
    in difference (castling pos) lost

calcBoard :: Position -> Move -> Board
calcBoard pos mv =
    let b = relocate pos (source mv) (destination mv)
        remove b a = delete a b
     in maybe b (remove b) (passantCapture pos mv)

relocate :: Position -> Square -> Square -> Board
relocate pos src dest = let b = board pos
                            p = fromJust (lookup src b)
                         in insert dest p $ delete src b

-- Calculate the square that was captured en passant by a move (if any)
passantCapture :: Position -> Move -> Maybe Square
passantCapture pos mv = 
    if Just dest == passant pos &&
       Just dest == ahead pos src &&
       adjacentFiles src dest &&
       pawnAt pos src &&
       maybe False (pawnAt pos) captureSquare &&
       maybe False (enemyAt pos) captureSquare
    then captureSquare
    else Nothing
    where captureSquare = back pos dest
          src = source mv
          dest = destination mv


isEmpty :: Position -> Square -> Bool
isEmpty pos sq = Nothing == pieceTypeAt pos sq

pawnAt :: Position -> Square -> Bool
pawnAt pos sq = Just Pawn == pieceTypeAt pos sq

kingAt :: Position -> Square -> Bool
kingAt pos sq = Just (Officer King) == pieceTypeAt pos sq

enemyAt :: Position -> Square -> Bool
enemyAt pos sq = Just (enemyColor pos) == colorAt pos sq

friendlyAt :: Position -> Square -> Bool
friendlyAt pos sq = Just (turn pos) == colorAt pos sq

pieceAt :: Position -> Square -> Maybe Piece
pieceAt = flip lookup. board

pieceTypeAt :: Position -> Square -> Maybe PieceType
pieceTypeAt pos sq = fmap pieceType (pieceAt pos sq)

colorAt :: Position -> Square -> Maybe Color
colorAt pos sq = fmap color (pieceAt pos sq)

enemyColor :: Position -> Color
enemyColor = toggle. turn

findSquare :: (Square -> Bool) -> Position -> Maybe Square
findSquare pred = find pred. occupiedSquares

occupiedSquares :: Position -> [Square]
occupiedSquares = keys. board

-- What castling right is lost if a piece moves from or to 'sq'?
lostCastling :: Square -> Maybe CastlingRight
lostCastling sq = let clr = case rank sq of
                                1 -> Just White
                                8 -> Just Black
                                _ -> Nothing

                      side = case file sq of
                                'a' -> Just Kingside
                                'h' -> Just Queenside
                                _ -> Nothing

                   in Castling <$> side <*> clr

adjacentFiles :: Square -> Square -> Bool
adjacentFiles sq1 sq2 = 1 == abs (fromEnum (file sq1) - fromEnum (file sq2))

toggle :: Color -> Color
toggle White = Black
toggle Black = White

backN :: Position -> Square -> Int -> Maybe Square
backN pos sq n = iterate (>>= back pos) (Just sq) !! n

aheadN :: Position -> Square -> Int -> Maybe Square
aheadN pos sq n = iterate (>>= ahead pos) (Just sq) !! n

ahead :: Position -> Square -> Maybe Square
ahead pos sq = if turn pos == White
               then up sq
               else down sq

back :: Position -> Square -> Maybe Square
back pos sq = if turn pos == Black
              then up sq
              else down sq

up :: Square -> Maybe Square
down :: Square -> Maybe Square
left :: Square -> Maybe Square
right :: Square -> Maybe Square
upLeft :: Square -> Maybe Square
upRight :: Square -> Maybe Square
downLeft :: Square -> Maybe Square
downRight :: Square -> Maybe Square

up sq = mv sq 1 0
down sq = mv sq (-1) 0
right sq = mv sq 0 1
left sq = mv sq 0 (-1)
upLeft = up >=> left
upRight = up >=> right
downLeft = down >=> left
downRight = down >=> right

inc :: Enum a => a -> Int -> a
inc x n = let op = if n < 0 then pred else succ
          in iterate op x !! n

mv :: Square -> Int -> Int -> Maybe Square
mv sq v h = let r' = inc (rank sq) v
                f' = inc (file sq) h
             in liftM2 Square (newFile f') (newRank r') 


newRank :: Int -> Maybe Int
newRank r = if validRank r then Just r else Nothing

newFile :: Char -> Maybe Char
newFile f = if validFile f then Just f else Nothing

validRank r = 1 <= r && r <= 8
validFile f = 'a' <= f && f <= 'h'

checkSource :: Position -> Move -> Maybe Error
checkSource pos mv  = let src = source mv
                          pclr = colorAt pos src
                       in maybe (Just NoPiece) (checkColorsMatch pos) pclr

checkColorsMatch :: Position -> Color -> Maybe Error
checkColorsMatch pos clr = if clr /= turn pos
                           then Just WrongColor
                           else Nothing

lastRank pos = case turn pos of
                    White -> 8
                    Black -> 1

maybeError :: Maybe Error -> a -> Either Error a
maybeError Nothing a = Right a
maybeError (Just err) _ = Left err

isRight (Right _) = True
isRight (Left _) = False

fAnd :: Applicative f => f Bool -> f Bool -> f Bool
fAnd a b = (&&) <$> a <*> b

