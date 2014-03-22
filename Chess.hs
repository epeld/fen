import Data.Map qualified
import Data.Set qualified

data Error = FilesNotAdjacent | 
             DestNotAheadBy Int | 
             KingCapturable | 
             NoPiece |
             WrongColor
             deriving (Show, Eq)

data Color = White | Black
             deriving (Show, Eq, Ord)

data Side = Queenside | Kingside
            deriving (Show, Eq)

data CastlingRight = Castling { side :: Side, color :: Color }
                     deriving (Show, Eq)

data Square = Square { file :: Char, Int :: rank }
              deriving (Show, Eq)

type Board = Data.Map.Map Square Piece

data Position = Position { board :: Board,
                           passant :: Maybe Square,
                           halfMoveNr :: Int,
                           fullMoveNr :: Int,
                           turn :: Color,
                           castling :: Data.Set.Set CastlingRight }
                           deriving (Show, Eq)

move :: Position -> Square -> Square -> Either Error Position
move pos source dest = let newPos = moveNaive pos source dest
                           errs = checkSource pos source <|> 
                                  checkLegal newPos
                        in if isNothing errs
                           then Right newPos
                           else Left KingCapturable
                            
-- Naive move: move disregarding king safety
moveNaive :: Position -> Square -> Square -> Either Error Position
moveNaive pos source dest = let t = pieceTypeAt pos source
                             in if t == Just Pawn
                                then moveNaivePawn pos source dest
                                else moveNaiveOfficer pos source dest

moveNaivePawn pos source dest = if capturableAt pos dest
                                then naivePawnCapture pos source dest
                                else naivePawnMove pos source dest

naivePawnTakes pos source dest = let errs = checkAdjacent source dest <|>
                                            checkAhead1 pos source dest
                                     newPos = performMove pos source dest
                                  in maybeEither errs newPos

-- peformMove is the function that actually performs the work;
-- moves the piece from source to dest and updates the position
performMove pos source dest = pos { turn = calcTurn pos,
                                    board = calcBoard pos source dest,
                                    fullMoveNr = calcFullMoveNr pos,
                                    halfMoveNr = calcHalfMoveNr pos source dest,
                                    passant = calcPassant pos source dest,
                                    castling = calcCastling pos source dest }


calcHalfMoveNr pos source dest = 0
calcPassant pos source dest = Nothing
calcFullMoveNr = (+1). fullMoveNr
calcTurn = toggle. turn

calcHalfMove pos source dest = if pawnAt source || enemyAt dest
                               then 0
                               else halfMoveNr pos + 1

calcCastling pos source dest = 
    let lost = fromList $ map lostCastling [source, dest]
    in Data.Set.difference (castling pos) lost

calcBoard pos source dest =
    let b = relocate (board pos) source dest
     in maybe b (flip' delete b) (passantCapture pos source dest)
                           
flip' f = (flip f $)

-- Calculate the square that was captured en passant by a move (if any)
passantCapture :: Position -> Square -> Square
passantCapture pos source dest = 
    let captureSquare = back pos dest
        isPassant = Just dest == passant pos &&
                    Just dest == ahead pos source &&
                    adjacentFiles source dest &&
                    pawnAt pos source &&
                    pawnAt pos =<< captureSquare &&
                    enemyAt pos =<< captureSquare
     in when isPassant captureSquare

when :: Bool -> Maybe a -> Maybe a
when a mb = if a then mb else Nothing


pawnAt :: Position -> Square -> Bool
pawnAt pos source = Just Pawn == pieceTypeAt pos source

enemyAt :: Position -> Square -> Bool
enemyAt pos source =  Just (enemyColor pos) == colorAt pos source

pieceTypeAt :: Position -> Square -> Maybe PieceType
pieceTypeAt pos source = pieceType =<< lookup source pos

enemyColor :: Position -> Color
enemyColor = toggle. turn

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

                   in liftM2 Castling

adjacentFiles :: Square -> Square -> Bool
adjacentFiles sq1 sq2 = 1 == abs (file sq1 - file sq2)

toggle :: Color -> Color
toggle White = Black
toggle Black = White

backN :: Position -> Square -> Int -> Maybe Square
backN pos sq n = take n $ iterate (>>= back) (Just sq)

aheadN :: Position -> Square -> Int -> Maybe Square
aheadN pos sq n = take n $ iterate (>>= ahead) (Just sq)

ahead pos sq = if turn pos == White
               then up sq
               else down sq

back pos sq = if turn pos == Black
              then up sq
              else down sq

up sq = mv sq 1 0
down sq = mv sq -1 0
right sq = mv sq 0 1
left sq = mv sq 0 -1

mv :: Square -> Int -> Int -> Maybe Square
mv sq v h = let r' = rank sq + v
                f' = file sq + h
             in liftM2 Square (newFile' f') (newRank r') 


newRank :: Int -> Maybe Int
newRank r = boolMaybe r (validRank r)

newFile :: Int -> Maybe Int
newFile f = boolMaybe f (validFile f)

validRank r = 1 <= r && r <= 8
validFile f = 'a' <= f && f <= 'h'

maybeEither :: Maybe a -> b -> Either a b
maybeEither ma b = maybe (Right b) Left ma

checkAhead1 = checkAheadN 1

checkAheadN :: Int -> Position -> Square -> Square -> Maybe Error
checkAheadN n pos source dest =
    boolMaybe (DestNotAheadBy n) (aheadN pos source n == Just dest)

checkAdjacent :: Square -> Square -> Maybe Error
checkAdjacent = boolMaybe FilesNotAdjacent. adjacentFiles

checkSource :: Position -> Square -> Maybe Error
checkSource pos source = let pclr = colorAt pos source
                          in maybe (Just NoPiece) checkColorsMatch

checkColorsMatch :: Position -> Color -> Maybe Error
checkColorsMatch = boolMaybe WrongColor. (pieceColor/=). turn

checkLegal :: Position -> Maybe Error
checkLegal = boolMaybe KingCapturable. legal

legal :: Position -> Bool
legal pos = True

boolMaybe :: a -> Bool -> Maybe a
boolMaybe a b = if b then Just a else Nothing
