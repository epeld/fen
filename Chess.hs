module Chess where
import Prelude hiding (lookup)
import Control.Applicative ((<|>), (<$>), (<*>), Applicative)
import Control.Monad (liftM2, (>=>), (>>=), (=<<))
import Control.Monad.Trans.Reader (ReaderT)
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
                   promotion :: Maybe OfficerType }
            deriving (Show, Eq)

type PositionReader = Reader Chess.Position 
type PositionReaderT = ReaderT Chess.Position 

move :: Move -> PositionReader (Either Error Position)
move mv = do p <- moveNaivePromote mv
             let legal = withReader (const p) isLegal
             maybeError legal p

-- Naive move := move disregarding king safety
moveNaivePromote :: Move -> PositionReader (Either Error Position)
moveNaivePromote mv = do checkPromotion mv
                         pos <- moveNaive mv
                         let promo = promotion mv
                         let promote = put $ destination mv
                         return (maybe id promote promo <$> pos)

moveNaive :: Move -> PositionReader (Either Error Position)
moveNaive pos mv = maybeError <$> errs <*> performMove pos mv
    where errs = firstError [checkSource mv, checkRange mv]

-------------------------------------------------------
-- Move Logic
-------------------------------------------------------

type Stepper = Square -> Maybe Square

type Sequence = [Square]
type Range = [Sequence]

type SequenceProducer = Square -> PositionReader Sequence
type RangeProducer = Square -> PositionReader Range

data RangeStrategy = TraverseEmpty |
                     FirstCapture
                     deriving (Show, Eq)

checkRange :: Move -> PositionReader (Maybe Error)
checkRange mv = choice <$> isInRange mv 
                       <*> pure Nothing 
                       <*> pure $ Just NotInRange

isInRange :: Move -> PositionReader Bool
isInRange mv = elem (destination mv) <$> flatten (fromMove mv)
                    

fromMove :: Move -> Range
fromMove mv = let pt = pieceTypeAt $ source mv
              in
              case pt of
                 Just t -> range t mv
                 Nothing -> []

fromMove' :: PieceType -> Move -> Range
fromMove' pt mv = let src = source mv
                  in choice <$> isCapture mv 
                            <*> threats pt src
                            <*> moves pt src

       
threats :: PieceType -> RangeProducer
threats = applied FirstCapture

moves :: PieceType -> RangeProducer
moves = applied TraverseEmpty

-- Applied, as opposed to theoretical range. See below
applied :: RangeStrategy -> PieceType -> RangeProducer
applied s pt = withStrategy s $ theoretical pt s


withStrategy :: RangeStrategy -> RangeProducer -> RangeProducer
withStrategy s r sq = map (withStrategy' s sq) <$> r sq

withStrategy' :: RangeStrategy -> SequenceProducer -> SequenceProducer
withStrategy' TraverseEmpty pr sq = takeWhileM isEmpty (pr sq)

-- TODO handle passant
withStrategy' FirstCapture pr sq = 
    do empty <- withStrategy' TraverseEmpty pr sq
       sqs <- pr sq
       let last = safeHead $ drop (length empty) sqs
       return $ case last of
           Nothing -> empty
           Just sq -> empty ++ (enemyAt sq >>= \y -> if y then [sq] else [])

-- Theoretical ranges. That is, 'in the best case'
theoretical :: PieceType -> RangeStrategy -> RangeProducer
theoretical Pawn TraverseEmpty = fromSteppers [forward] >>= 
                                 map (take 2)

theoretical Pawn FirstCapture = fromSteppers [forward >=> left, 
                                              forward >=> right] >>= 
                                map (take 1)

theoretical (Officer o) _ = theoretical' o 

theoretical' :: PieceType -> RangeProducer
theoretical' Bishop = fromSteppers [upLeft, upRight, downLeft, downRight]
theoretical' Rook = fromSteppers [up, down, left, right]
theoretical' Queen = sequence [theoretical' Bishop, theoretical Rook s]
theoretical' King = map (take 1) =<< theoretical' Queen
theoretical' Knight = fromSteppers knightSteppers
          
knightSteppers = foldl' (>=>) return $
    [[up, up, left], [up, up, right], 
     [up, right, right], [down, right, right],
     [down, down, right], [down, down, left],
     [down, left, left], [up, left, left]]

------------------------------------------
-- Position Construction
       
performMove :: Move -> PositionReader Position
performMove mv = 
    Position <$> calcBoard mv 
             <*> calcPassant mv
             <*> calcHalfMoveNr mv
             <*> calcFullMoveNr
             <*> calcTurn
             <*> calcCastling mv
    

calcHalfMoveNr mv = choice <$> anyM [pawnAt $ source mv,
                                     isCapture mv]
                           <*> pure 0
                           <*> liftM (inc 1. halfMoveNr) ask

calcPassant mv = 
    let src = source mv
        dest = dest = destination mv 
    in
    choice <$> everyM [pawnAt src, 
                       isForward 2 src dest,
                       onInitialRank src]
           <*> back src
           <*> Nothing
          

calcFullMoveNr :: PositionReader Int
calcFullMoveNr = liftM (inc 1. fullMoveNr) ask

calcTurn :: PositionReader Color
calcTurn = liftM (toggle. turn) ask

calcCastling :: Move -> PositionReader (Set CastlingRight)
calcCastling mv = 
    let lost = Data.Set.fromList $ 
            mapMaybe lostCastling [source mv, destination mv]
    in do
        c <- liftM castling ask
        difference c lost

calcBoard :: Move -> PositionReader Board
calcBoard mv =
    fromMaybe id delete <$> passantCapture mv 
                        <*> relocate (source mv) (destination mv)

------------------------------------
-- Square Stepping (Construction)

backN :: Int -> Square -> PositionReader (Maybe Square)
backN n sq = iterate (>>= back) (Just sq) !! n

forward :: Int -> Square -> PositionReader (Maybe Square)
aheadN n sq = iterate (>>= forward) (Just sq) !! n

forward :: Square -> PositionReader (Maybe Square)
forward sq = choice <$> liftM turn ask
                    <*> up sq
                    <*> down sq

back :: Square -> PositionReader (Maybe Square)
back sq = local toggleTurn forward
              

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

---------------------------------
-- Checks

checkSource :: Move -> PositionReader (Maybe Error)
checkSource mv = let c = colorAt (source mv)
                  in checks [liftM (flipMaybe NoPiece) c, 
                             checkColorsMatch =<< c]

checkColorsMatch :: Color -> PositionReader (Maybe Error)
checkColorsMatch clr = choice <$> liftM ((clr /=). turn) ask
                              <*> pure $ Just WrongColor
                              <*> pure Nothing

checkPromotion :: Move -> PositionReader (Maybe Error)
checkPromotion mv = shouldPromote mv >>= checkPromotes should mv
           
checkPromotes mv should = 
    let promo = promotion mv
    in if should 
       then flipMaybe promo PromotionRequired 
       else promo >> Just IllegalPromotion



checkPromotedPawns :: PositionReader (Maybe Error)
checkPromotedPawns = if any (onLastRank pos) pawnSquares
                     then Just PromotionRequired
                     else Nothing
    where pawnSquares = filter (pawnAt pos) (friendlySquares pos)


checkKingSafe :: PositionReader (Maybe Error)
checkKingSafe = maybe (Just MissingKing) <$> checkAttacked <*> kingSquare

checkAttacked :: Square -> PositionReader (Maybe Error)
checkAttacked pos sq = do attacked <- isAttacked sq
                          return if attacked 
                                 then Just KingCapturable 
                                 else Nothing

----------------------------------
-- Chess Utils
--
--

isCapture :: Move -> PositionReader Bool
isCapture pos mv = everyM [enemyAt (destination mv), isPassantCapture mv]

isPassantCapture :: Move -> PositionReader Bool
isPassantCapture pos mv = isJust (passantCapture pos mv)


enemyPawnAt :: Square -> PositionReader Bool
enemyPawnAt pos dest = everyM [enemyAt dest, pawnAt dest]
                                           

shouldPromote :: Move -> PositionReader Bool
shouldPromote mv = everyM [pawnAt $ source mv, onLastRank $ destination mv]

kingSquare :: PositionReader (Maybe Square)
kingSquare pos = findSquare (kingAt pos `fAnd` enemyAt pos) pos

isAttacked :: Square -> PositionReader Bool
isAttacked sq = any <$> couldMove <*> friendlySquares
    where couldMove :: Square -> Bool
          couldMove src = canMoveNaive (Move { source = src,
                                               destination = sq,
                                               promotion = Nothing })

canMoveNaive :: Move -> PositionReader Bool
canMoveNaive pos mv = isRight <$> moveNaive mv

friendlySquares :: PositionReader [Square]
friendlySquares = filterM friendlyAt occupiedSquares

occupiedSquares :: PositionReader [Square]
occupiedSquares = liftM (keys. board)

isLegal :: PositionReader (Maybe Error)
isLegal = firstError [checkKingSafe, checkPromotedPawns]

onLastRank sq = do last <- lastRank
                   rank sq == last

onInitialRank sq = do initial <- initialRank
                      rank sq == initial


initialRank :: PositionReader Int
initialRank = do clr <- liftM turn ask
                 case turn pos of
                    White -> 2
                    Black -> 7

lastRank :: PositionReader Int
lastRank = do clr <- liftM turn ask
              case turn pos of
                  White -> return 8
                  Black -> return 1

relocate :: Square -> Square -> PositionReader Board
relocate src dest = liftM (delete src) (copy src dest)

copy :: Square -> Square -> PositionReader Board
copy src dest = do let b = liftM board ask
                   pc <- liftM (lookup src) b
                   case pc of
                       Just t -> liftM (insert src t) b
                       Nothing -> b

-- Calculate the square that was captured en passant by a move (if any)
passantCapture :: Move -> PositionReader (Maybe Square)
passantCapture mv = 
    let src = source mv
    let dest = destination mv
    captureSquare <- back dest
    plausible <- everyM [pawnAt dest,
                         maybeNot pawnAt captureSquare,
                         maybeNot pawnAt pos captureSquare,
                         isPassantSquare dest, 
                         isAhead src dest]
    return $ 
        if adjacentFiles src dest && plausible
        then captureSquare
        else Nothing

maybeNot = maybe (return False)

isEmpty :: Square -> PositionReader Bool
isEmpty sq = liftM isNothing (pieceTypeAt sq)

pawnAt :: Square -> Bool
pawnAt sq = liftM (Just Pawn ==) (pieceTypeAt sq)

kingAt :: Square -> PositionReader Bool
kingAt sq = liftM (Just (Officer King) ==) (pieceTypeAt sq)

enemyAt :: Square -> PositionReader Bool
enemyAt pos sq = liftM2 (==) (colorAt pos sq) enemyColor

friendlyAt :: Square -> PositionReader Bool
friendlyAt pos sq = liftM2 (==) (turn =<< ask) (colorAt sq)

pieceAt :: Square -> PositionReader (Maybe Piece)
pieceAt sq = liftM (lookup sq. board) ask

pieceTypeAt :: Square -> PositionReader (Maybe PieceType)
pieceTypeAt sq = do pc <- pieceAt sq
                    return $ fmap pieceType pc

colorAt :: Square -> Positionreader (Maybe Color)
colorAt sq = do pc <- pieceAt sq
                return $ fmap color pc

enemyColor :: PositionReader Color
enemyColor = liftM (toggle. turn)

findSquare :: (Square -> PositionReader Bool) -> PositionReader (Maybe Square)
findSquare pred = fmap safeHead (filterM pred =<< occupiedSquares)

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

toggleTurn :: Position -> Position
toggleTurn p = p{turn = toggle $ turn p }

-------------------------------------
-- General Utils

safeHead [] = Nothing
safeHead xs = Just $ head xs

maybeError :: Maybe Error -> a -> Either Error a
maybeError Nothing a = Right a
maybeError (Just err) _ = Left err

isRight (Right _) = True
isRight (Left _) = False

flipMaybe a b = case a of
    Nothing -> Just b
    Just _ -> Nothing

firstError :: [PositionReader (Maybe Error)] -> PositionReader (Maybe Error)
firstError = getFirst. checks First. map (First <$>)

checks :: Monoid m => 
          m -> [PositionReader (Maybe Error)] -> PositionReader (Maybe Error)
checks = fmap mconcat. sequence

takeWhileM :: Monad m => (a -> m Bool) -> [a] -> m [a]
takeWhileM f xs = liftM (flip take xs. length. takeWhile id) $ seqmap f xs

seqmap f = sequence. map f

choice :: Bool -> a -> a
choice a b c = if a then b else c

