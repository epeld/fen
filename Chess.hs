import Prelude hiding (lookup)
import Control.Applicative ((<|>), (<$>), (<*>))
import Control.Monad (liftM2, (>=>), (>>=), (=<<))
import Data.Map (insert, delete, keys, Map, lookup)
import Data.List (find)
import Data.Set (fromList, Set, difference)
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe)

data Error = FilesNotAdjacent | 
             DestNotAheadBy Int | 
             KingCapturable | 
             NoPiece |
             WrongColor |
             NotInRange
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


move :: Position -> Square -> Square -> Either Error Position
move pos source dest = 
    moveNaive pos source dest >>= \newPos ->
        if isLegal newPos
        then Right newPos
        else Left KingCapturable

isLegal pos = let ks = findSquare (kingAt pos `fAnd` enemyAt pos) pos
                  fAnd = liftM2 (&&)
               in maybe False (\ks -> isAttacked pos ks) ks

isAttacked :: Position -> Square -> Bool
isAttacked pos sq = any (\x -> canMoveNaive pos x sq) friendlySquares
    where friendlySquares = filter (friendlyAt pos) (keys $ board pos)

                           
                            
-- Naive move := move disregarding king safety
canMoveNaive :: Position -> Square -> Square -> Bool
canMoveNaive pos source dest = case moveNaive pos source dest of
    Left _ -> False
    Right _ -> True

moveNaive :: Position -> Square -> Square -> Either Error Position
moveNaive pos source dest = let errs = checkSource pos source <|>
                                       checkRange pos source dest
                                newPos = performMove pos source dest
                            in maybeEither errs newPos


checkRange pos source dest = case pieceTypeAt pos source of
    Nothing -> Just NoPiece
    Just pt -> let range = applicableRange pt pos source dest
               in if reaches pos dest range
               then Nothing
               else Just NotInRange


applicableRange pt pos source dest = if capturableBy pt pos dest
                                     then captureRange pt pos dest
                                     else moveRange pt pos dest


captureRange :: PieceType -> Position -> Square -> [[Square]]
captureRange Pawn pos source = let fw = ahead pos source
                                   sqs = [ left =<< fw, right =<< fw ] 
                                in map (:[]) (catMaybes sqs)
captureRange pt pos source = moveRange pt pos source

moveRange :: PieceType -> Position -> Square -> [[Square]]
moveRange Pawn pos source = [ catMaybes [ aheadN pos source 1,
                                          aheadN pos source 2 ]]

moveRange (Officer o) pos source = moveRange' o pos source
moveRange' Bishop _ s = applySteppers [ upLeft, upRight, downLeft, downRight ] s
moveRange' Rook _ s = applySteppers [ up, down, left, right ] s
moveRange' Queen p s = moveRange' Bishop p s ++ moveRange' Rook p s
moveRange' King p s = map (take 1) (moveRange' Queen p s)
moveRange' Knight p s = map (:[]) (knightSquares s)

knightSquares :: Square -> [Square]
knightSquares s = stepSteppers [ up >=> up >=> left,
                                 up >=> up >=> right,
                                 up >=> right >=> right,
                                 down >=> right >=> right,
                                 down >=> down >=> right,
                                 down >=> down >=> left,
                                 down >=> left >=> left,
                                 up >=> left >=> left ] s


type Stepper = Square -> Maybe Square

-- Make one step with each stepper
stepSteppers :: [Stepper] -> Square -> [Square]
stepSteppers steppers source = catMaybes $ map ($ source) steppers

-- Run a stepper until it doesn't produce squares anymore
applyStepper stepper s = catMaybes $ takeWhile isJust $ drop 1 $
                         iterate (>>= stepper) (Just s)

applySteppers :: [Stepper] -> Square -> [[Square]]
applySteppers steppers source = applyStepper <$> steppers <*> [source]

reaches :: Position -> Square -> [[Square]] -> Bool
reaches pos dest = any $ emptyUntil pos dest
    where emptyUntil :: Position -> Square -> [Square] -> Bool
          emptyUntil pos sq series = let checkees = takeWhile (/= sq) series
                                     in sq `elem` series &&
                                        all (isEmpty pos) checkees
       
capturableBy Pawn pos dest = enemyAt pos dest || capturableEnPassant pos dest
capturableBy _ pos dest = enemyAt pos dest

capturableEnPassant pos dest = let target = back pos dest
                                in Just dest == passant pos &&
                                   maybe False (enemyPawnAt pos) target
                                   

enemyPawnAt pos dest = enemyAt pos dest && pawnAt pos dest
                                           

-- peformMove is the function that actually performs the work;
-- moves the piece from source to dest and updates the position
performMove pos source dest = pos { turn = calcTurn pos,
                                    board = calcBoard pos source dest,
                                    fullMoveNr = calcFullMoveNr pos,
                                    halfMoveNr = calcHalfMoveNr pos source dest,
                                    passant = calcPassant pos source dest,
                                    castling = calcCastling pos source dest }


calcHalfMoveNr pos source dest = 0
calcPassant pos source dest = 
    if pieceTypeAt pos source == Just Pawn &&
       Just dest == aheadN pos source 2 &&
       rank source == initialPawnRank
    then back pos source
    else Nothing
    where initialPawnRank :: Int
          initialPawnRank = case turn pos of
              White -> 2
              Black -> 7

calcFullMoveNr :: Position -> Int
calcFullMoveNr = (+1). fullMoveNr

calcTurn :: Position -> Color
calcTurn = toggle. turn

calcHalfMove :: Position -> Square -> Square -> Int
calcHalfMove pos source dest = if pawnAt pos source || enemyAt pos dest
                               then 0
                               else halfMoveNr pos + 1

calcCastling :: Position -> Square -> Square -> Set CastlingRight
calcCastling pos source dest = 
    let lost = Data.Set.fromList $ mapMaybe lostCastling [source, dest]
    in difference (castling pos) lost

calcBoard :: Position -> Square -> Square -> Board
calcBoard pos source dest =
    let b = relocate pos source dest
     in maybe b (flip' delete b) (passantCapture pos source dest)

relocate pos source dest = let b = board pos
                               p = mustBeJust (lookup source b) "relocate"
                             in insert dest p $ delete source b

mustBeJust mb msg = fromMaybe (error msg) mb
                           
flip' f = (flip f $)

-- Calculate the square that was captured en passant by a move (if any)
passantCapture :: Position -> Square -> Square -> Maybe Square
passantCapture pos source dest = 
    let captureSquare = back pos dest
        isPassant = Just dest == passant pos &&
                    Just dest == ahead pos source &&
                    adjacentFiles source dest &&
                    pawnAt pos source &&
                    maybe False (pawnAt pos) captureSquare &&
                    maybe False (enemyAt pos) captureSquare
     in when isPassant captureSquare

when :: Bool -> Maybe a -> Maybe a
when a mb = if a then mb else Nothing


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
findSquare pred pos = find pred (keys $ board pos)

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

                   in liftM2 Castling side clr

adjacentFiles :: Square -> Square -> Bool
adjacentFiles sq1 sq2 = 1 == abs (fromEnum (file sq1) - fromEnum (file sq2))

toggle :: Color -> Color
toggle White = Black
toggle Black = White

backN :: Position -> Square -> Int -> Maybe Square
backN pos sq n = iterate (>>= back pos) (Just sq) !! n

aheadN :: Position -> Square -> Int -> Maybe Square
aheadN pos sq n = iterate (>>= ahead pos) (Just sq) !! n

ahead pos sq = if turn pos == White
               then up sq
               else down sq

back pos sq = if turn pos == Black
              then up sq
              else down sq

up :: Square -> Maybe Square
down :: Square -> Maybe Square
left :: Square -> Maybe Square
right :: Square -> Maybe Square

up sq = mv sq 1 0
down sq = mv sq (-1) 0
right sq = mv sq 0 1
left sq = mv sq 0 (-1)

upLeft = up >=> left
upRight = up >=> right
downLeft = down >=> left
downRight = down >=> right

inc x n = let op = if n < 0 then pred else succ
          in iterate op x !! n

mv :: Square -> Int -> Int -> Maybe Square
mv sq v h = let r' = inc (rank sq) v
                f' = inc (file sq) h
             in liftM2 Square (newFile f') (newRank r') 


newRank :: Int -> Maybe Int
newRank r = boolMaybe r (validRank r)

newFile :: Char -> Maybe Char
newFile f = boolMaybe f (validFile f)

validRank r = 1 <= r && r <= 8
validFile f = 'a' <= f && f <= 'h'

maybeEither :: Maybe a -> b -> Either a b
maybeEither ma b = maybe (Right b) Left ma


checkSource :: Position -> Square -> Maybe Error
checkSource pos source = let pclr = colorAt pos source
                          in maybe (Just NoPiece) (checkColorsMatch pos) pclr

checkColorsMatch :: Position -> Color -> Maybe Error
checkColorsMatch pos clr = boolMaybe WrongColor (clr /= turn pos)


boolMaybe :: a -> Bool -> Maybe a
boolMaybe a b = if b then Just a else Nothing
