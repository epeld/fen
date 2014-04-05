module Chess where
import Prelude hiding (lookup)
import Control.Applicative ((<|>), (<$>), (<*>), pure, Applicative)
import Control.Monad (liftM, liftM2, filterM, (>=>), (>>=), (=<<))
import Control.Monad.Reader (Reader)
import Control.Monad.Trans.Reader (runReader, ReaderT, ask, local)
import Data.Monoid (mappend, mconcat, First(..), getFirst, Monoid)
import Data.Map (insert, delete, keys, Map, lookup)
import Data.List (find, foldl')
import Data.Set (fromList, Set, difference)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, fromJust, mapMaybe)

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
             return $ case p of
                 Right pos -> maybeError (runReader isLegal pos) pos
                 Left err -> Left err

-- Naive move := move disregarding king safety
moveNaivePromote :: Move -> PositionReader (Either Error Position)
moveNaivePromote mv = do checkPromotion mv
                         pos <- moveNaive mv
                         let promo = promotion mv
                         let dest = destination mv
                         return $ case promo of
                             Nothing -> pos
                             Just pr -> runReader (promote dest pr) <$> pos

moveNaive :: Move -> PositionReader (Either Error Position)
moveNaive mv = maybeError <$> errs <*> performMove mv
    where errs = firstError [checkSource mv, checkRange mv]

-------------------------------------------------------
-- Move Logic
-------------------------------------------------------

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
                       <*> pure (Just NotInRange)

isInRange :: Move -> PositionReader Bool
isInRange mv = elem (destination mv). concat <$> fromMove mv
                    

fromMove :: Move -> PositionReader Range
fromMove mv = do
    pt <- pieceTypeAt $ source mv
    case pt of
       Just t -> fromMove' t mv
       Nothing -> return []

fromMove' :: PieceType -> Move -> PositionReader Range
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
applied s pt sq = withStrategy s =<< theoretical pt s sq


withStrategy :: RangeStrategy -> Range -> PositionReader Range
--                 (->)   Reader []
withStrategy s r = sequence $ map (withStrategy' s) r

withStrategy' :: RangeStrategy -> Sequence -> PositionReader Sequence
withStrategy' TraverseEmpty sqs = takeWhileM isEmpty sqs


-- TODO handle passant
withStrategy' FirstCapture sqs = 
    do empty <- withStrategy' TraverseEmpty sqs
       let last = safeHead $ drop (length empty) sqs
       case last of
           Nothing -> return empty
           Just sq -> liftM (empty ++) (choice <$> enemyAt sq 
                                               <*> return [sq]
                                               <*> return [])

-- Theoretical ranges. That is, 'in the best case'
theoretical :: PieceType -> RangeStrategy -> RangeProducer
theoretical (Officer o) _ = theoretical' o 

-- TODO clean up this mess..
theoretical Pawn TraverseEmpty = \s -> 
    do sqs <- sequence [forwardN 1 s, forwardN 2 s]
       (:[]). catMaybes <$> takeWhileM (return. isJust) sqs
       
theoretical Pawn FirstCapture = \s ->
    do next <- forward s
       return $ map (:[]) $ catMaybes [next >>= left, next >>= right]

theoretical' :: OfficerType -> RangeProducer
theoretical' King = fromSteppersN 1 $ steppers King
theoretical' Knight = fromSteppersN 1 $ steppers Knight
theoretical' pt = fromSteppers $ steppers pt
          
steppers :: OfficerType -> [Stepper]
steppers King = steppers Queen
steppers Queen = concat $ [steppers Rook, steppers Bishop]
steppers Rook = [up, down, left, right]
steppers Bishop = [upLeft, upRight, downLeft, downRight]
steppers Knight = foldl' (>=>) return <$>
    [[up, up, left], [up, up, right], 
     [up, right, right], [down, right, right],
     [down, down, right], [down, down, left],
     [down, left, left], [up, left, left]]

---------------------
-- Steppers

type Stepper = Square -> Maybe Square

runStepperOnce :: Stepper -> Square -> Maybe Square
runStepperOnce stepper = safeHead. take 1. runStepper stepper

runStepper :: Stepper -> Square -> [Square]
runStepper stepper sq = 
    case stepper sq of
        Just sq' -> sq' : runStepper stepper sq'
        Nothing -> []

fromSteppers :: [Stepper] -> RangeProducer
fromSteppers steppers = return. sequence (map runStepper steppers)

fromSteppersN :: Int -> [Stepper] -> RangeProducer
fromSteppersN n steppers = liftM (map $ take n). fromSteppers steppers

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
    

calcHalfMoveNr mv = choice <$> someM [pawnAt $ source mv,
                                      isCapture mv]
                           <*> pure 0
                           <*> liftM (inc 1. halfMoveNr) ask

calcPassant :: Move -> PositionReader (Maybe Square)
calcPassant mv = 
    let src = source mv
        dest = destination mv 
    in
    choice <$> everyM [pawnAt src, 
                       isForward 2 src dest,
                       onInitialRank src]
           <*> back src
           <*> pure Nothing
          

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
        return $ difference c lost

calcBoard :: Move -> PositionReader Board
calcBoard mv = do
    let b = relocate (source mv) (destination mv)
    capture <- passantCapture mv 
    case capture of
        Nothing -> b
        Just sq -> liftM (delete sq) b

------------------------------------
-- Square Stepping (Construction)

backN :: Int -> Square -> PositionReader (Maybe Square)
backN n sq = local toggleTurn (forwardN n sq)

forwardN :: Int -> Square -> PositionReader (Maybe Square)
forwardN 0 sq = return $ Just sq
forwardN n sq = do next <- forward sq 
                   case next of
                       Just sq' -> forwardN (n - 1) sq'
                       Nothing -> return Nothing

forward :: Square -> PositionReader (Maybe Square)
forward sq = choice <$> liftM ((White ==). turn) ask
                    <*> return (up sq)
                    <*> return (down sq)

back :: Square -> PositionReader (Maybe Square)
back = local toggleTurn. forward
              

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
                  in firstError [liftM (flipMaybe NoPiece) c, 
                                 checkColorsMatch. fromJust =<< c]

checkColorsMatch :: Color -> PositionReader (Maybe Error)
checkColorsMatch clr = choice <$> liftM ((clr /=). turn) ask
                              <*> pure (Just WrongColor)
                              <*> pure Nothing

checkPromotion :: Move -> PositionReader (Maybe Error)
checkPromotion mv = liftM (checkPromotes mv) (shouldPromote mv)
           
checkPromotes :: Move -> Bool -> Maybe Error
checkPromotes mv should = 
    let promo = promotion mv
    in if should 
       then flipMaybe PromotionRequired promo
       else promo >> Just IllegalPromotion



checkPromotedPawns :: PositionReader (Maybe Error)
checkPromotedPawns = choice <$> (anyM onLastRank =<< pawnSquares)
                            <*> pure (Just PromotionRequired)
                            <*> pure Nothing
    where pawnSquares = filterM pawnAt =<< friendlySquares


checkKingSafe :: PositionReader (Maybe Error)
checkKingSafe = do
    ks <- kingSquare
    case ks of
        Nothing -> return $ Just MissingKing
        Just s -> checkAttacked s

checkAttacked :: Square -> PositionReader (Maybe Error)
checkAttacked sq = choice <$> isAttacked sq 
                          <*> pure (Just KingCapturable)
                          <*> pure Nothing

----------------------------------
-- Chess Utils
--
--

promote :: Square -> OfficerType -> PositionReader Position
promote sq pr = do
    pos <- ask
    let pc = Piece (Officer pr) (turn pos) 
    return pos{ board = insert sq pc (board pos) }


isCapture :: Move -> PositionReader Bool
isCapture mv = everyM [enemyAt $ destination mv,
                       isPassantCapture mv]

isPassantCapture :: Move -> PositionReader Bool
isPassantCapture mv = liftM isJust (passantCapture mv)


enemyPawnAt :: Square -> PositionReader Bool
enemyPawnAt dest = everyM [enemyAt dest, pawnAt dest]
                                           

shouldPromote :: Move -> PositionReader Bool
shouldPromote mv = everyM [pawnAt $ source mv, onLastRank $ destination mv]

kingSquare :: PositionReader (Maybe Square)
kingSquare = findSquare $ everyM. sequence [kingAt, enemyAt]

isAttacked :: Square -> PositionReader Bool
isAttacked sq = anyM couldMove =<< friendlySquares
    where couldMove :: Square -> PositionReader Bool
          couldMove src = canMoveNaive (Move { source = src,
                                               destination = sq,
                                               promotion = Nothing })

canMoveNaive :: Move -> PositionReader Bool
canMoveNaive mv = isRight <$> moveNaive mv

friendlySquares :: PositionReader [Square]
friendlySquares = filterM friendlyAt =<< occupiedSquares

occupiedSquares :: PositionReader [Square]
occupiedSquares = liftM (keys. board) ask

isLegal :: PositionReader (Maybe Error)
isLegal = firstError [checkKingSafe, checkPromotedPawns]

onLastRank sq = do last <- lastRank
                   return $ rank sq == last

onInitialRank sq = do initial <- initialRank
                      return $ rank sq == initial


initialRank :: PositionReader Int
initialRank = do clr <- liftM turn ask
                 return $ case clr of
                    White -> 2
                    Black -> 7

lastRank :: PositionReader Int
lastRank = do clr <- liftM turn ask
              case clr of
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
passantCapture mv = do
    let src = source mv
    let dest = destination mv
    captureSquare <- back dest
    plausible <- everyM [pawnAt dest,
                         maybeNot pawnAt captureSquare,
                         maybeNot enemyAt captureSquare,
                         isPassantSquare dest, 
                         isForward 1 src dest]
    return $ 
        if adjacentFiles src dest && plausible
        then captureSquare
        else Nothing

isPassantSquare :: Square -> PositionReader Bool
isPassantSquare sq = liftM ((Just sq ==). passant) ask

isForward :: Int -> Square -> Square -> PositionReader Bool
isForward n src dst = liftM (Just dst ==) (forwardN n src)


isEmpty :: Square -> PositionReader Bool
isEmpty sq = liftM isNothing (pieceTypeAt sq)

pawnAt :: Square -> PositionReader Bool
pawnAt sq = liftM (Just Pawn ==) (pieceTypeAt sq)

kingAt :: Square -> PositionReader Bool
kingAt sq = liftM (Just (Officer King) ==) (pieceTypeAt sq)

enemyAt :: Square -> PositionReader Bool
enemyAt sq = liftM2 (==) (Just <$> enemyColor) (colorAt sq) 

friendlyAt :: Square -> PositionReader Bool
friendlyAt sq = liftM2 (==) (Just. turn <$> ask) (colorAt sq)

pieceAt :: Square -> PositionReader (Maybe Piece)
pieceAt sq = liftM (lookup sq. board) ask

pieceTypeAt :: Square -> PositionReader (Maybe PieceType)
pieceTypeAt sq = do pc <- pieceAt sq
                    return $ fmap pieceType pc

colorAt :: Square -> PositionReader (Maybe Color)
colorAt sq = do pc <- pieceAt sq
                return $ fmap color pc

enemyColor :: PositionReader Color
enemyColor = liftM (toggle. turn) ask

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

maybeNot = maybe (return False)

safeHead [] = Nothing
safeHead xs = Just $ head xs

maybeError :: Maybe Error -> a -> Either Error a
maybeError Nothing a = Right a
maybeError (Just err) _ = Left err

isRight (Right _) = True
isRight (Left _) = False

flipMaybe a b = case b of
    Nothing -> Just a
    Just _ -> Nothing

firstError :: [PositionReader (Maybe Error)] -> PositionReader (Maybe Error)
firstError = liftM (getFirst. mconcat. map First). sequence

takeWhileM :: Monad m => (a -> m Bool) -> [a] -> m [a]
takeWhileM f xs = liftM (flip take xs. length. takeWhile id) $ seqmap f xs

seqmap f = sequence. map f

choice :: Bool -> a -> a -> a
choice a b c = if a then b else c

anyM :: (a -> PositionReader Bool) -> [a] -> PositionReader Bool
anyM f [] = return True
anyM f xs = liftM (not. null) (filterM f xs)

everyM :: [PositionReader Bool] -> PositionReader Bool
everyM = fmap (all id). sequence

someM :: [PositionReader Bool] -> PositionReader Bool
someM xs = anyM id xs

-- The holy grail:
swapM :: (a -> PositionReader b) -> PositionReader (a -> b)
swapM r = liftM (\x -> liftM (flip runReader x) r) ask
