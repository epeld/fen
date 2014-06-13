module Chess where
import Prelude hiding (lookup)
import Control.Applicative ((<|>), (<$>), (<*>), pure, Applicative)
import Control.Monad (liftM, liftM2, filterM, (>=>), (>>=), (=<<))
import Control.Monad.Reader (Reader)
import Control.Monad.Trans.Reader (runReader, ReaderT, ask, local)
import Data.Monoid (mappend, mconcat, First(..), getFirst, Monoid)
import Data.Map (insert, delete, keys, Map, lookup)
import Data.List (find, foldl')
import Data.Set (fromList, union, Set, difference)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, fromJust, mapMaybe)
import Types
import Utils

move :: Move -> PositionReader (Either Error Position)
move mv = do p <- moveNaivePromote mv
             return $ case p of
                 Right pos -> maybeError (runReader checkLegal pos) pos
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
                  in if_ <$> isCapture mv <*> threats pt src <*> moves pt src


threats :: PieceType -> RangeProducer
threats = applied FirstCapture

moves :: PieceType -> RangeProducer
moves = applied TraverseEmpty

-- Applied, as opposed to theoretical range. See below
-- applied handles passant, applied' does not
applied :: RangeStrategy -> PieceType -> RangeProducer
applied FirstCapture Pawn sq =
  liftM2 (++) (passantRange sq) (applied' FirstCapture Pawn sq)
applied s pt sq = applied' s pt sq

-- Note: applied' does not handle the passant rule. See above
applied' :: RangeStrategy -> PieceType -> RangeProducer
applied' s pt sq = withStrategy s =<< theoretical pt s sq

passantRange :: RangeProducer
passantRange sq = do sqs <- concat <$> theoretical Pawn FirstCapture sq
                     p <- passant <$> ask
                     return $ case p of
                       Nothing -> []
                       Just ps -> if elem ps sqs then [[ps]] else []


withStrategy :: RangeStrategy -> Range -> PositionReader Range
withStrategy s = mapM (withStrategy' s)

withStrategy' :: RangeStrategy -> Sequence -> PositionReader Sequence
withStrategy' TraverseEmpty sqs = takeWhileM isEmpty sqs


withStrategy' FirstCapture sqs =
    do empty <- withStrategy' TraverseEmpty sqs
       let last = safeHead $ drop (length empty) sqs
       case last of
           Nothing -> return empty
           Just sq -> liftM (empty ++) (ifM1 (enemyAt sq) [sq] [])

-- Theoretical ranges. That is, 'in the best case'
theoretical :: PieceType -> RangeStrategy -> RangeProducer
theoretical (Officer o) _ = theoretical' o

theoretical Pawn TraverseEmpty =
  \s -> do stepper <- swapM forward
           fromStepperN 2 stepper s

theoretical Pawn FirstCapture =
  \s -> do stepper <- swapM forward
           let steppers = [stepper >=> left, stepper >=> right]
           fromSteppersN 1 steppers s

fromStepperN n = fromSteppersN n. (:[])

theoretical' :: OfficerType -> RangeProducer
theoretical' King = fromSteppersN 1 $ steppers King
theoretical' Knight = fromSteppersN 1 $ steppers Knight
theoretical' pt = fromSteppers $ steppers pt


---------------------
-- Steppers

type Stepper = Square -> Maybe Square

steppers :: OfficerType -> [Stepper]
steppers King = steppers Queen
steppers Queen = steppers Rook ++ steppers Bishop
steppers Rook = [up, down, left, right]
steppers Bishop = [upLeft, upRight, downLeft, downRight]
steppers Knight = foldl' (>=>) return <$>
    [[up, up, left], [up, up, right],
     [up, right, right], [down, right, right],
     [down, down, right], [down, down, left],
     [down, left, left], [up, left, left]]

runStepperOnce :: Stepper -> Square -> Maybe Square
runStepperOnce stepper = safeHead. take 1. runStepper stepper

runStepper :: Stepper -> Square -> [Square]
runStepper stepper sq =
    case stepper sq of
        Just sq' -> sq' : runStepper stepper sq'
        Nothing -> []

fromSteppers :: [Stepper] -> RangeProducer
fromSteppers steppers = return. mapM runStepper steppers

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


calcHalfMoveNr mv = if_ <$> someM [pawnAt $ source mv,
                                   isCapture mv]
                        <*> pure 0
                        <*> liftM (inc 1. halfMoveNr) ask

calcPassant :: Move -> PositionReader (Maybe Square)
calcPassant mv =
    let src = source mv
        dest = destination mv
    in
    if_ <$> everyM [pawnAt src, isForward 2 src dest, onInitialRank src]
        <*> back src
        <*> pure Nothing


calcFullMoveNr :: PositionReader Int
calcFullMoveNr = liftM (inc 1. fullMoveNr) ask

calcTurn :: PositionReader Color
calcTurn = liftM (toggle. turn) ask

calcCastling :: Move -> PositionReader (Set CastlingRight)
calcCastling mv =
    let lost = union (lostCastling $ source mv) (lostCastling $ destination mv)
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
forward sq = ifM1 (liftM ((White ==). turn) ask)
                  (up sq)
                  (down sq)

back :: Square -> PositionReader (Maybe Square)
back = local toggleTurn. forward

inc :: Enum a => a -> Int -> a
inc x n = let op = if n < 0 then pred else succ
          in iterate op x !! abs n


---------------------------------
-- Checks

checkRange :: Move -> PositionReader (Maybe Error)
checkRange mv = justUnlessM1 (isInRange mv) NotInRange

checkSource :: Move -> PositionReader (Maybe Error)
checkSource mv = let c = colorAt (source mv)
                  in firstError [liftM (flipMaybe NoPiece) c,
                                 checkColorsMatch. fromJust =<< c]

checkColorsMatch :: Color -> PositionReader (Maybe Error)
checkColorsMatch clr = justWhenM1 (liftM ((clr /=). turn) ask) WrongColor


checkPromotion :: Move -> PositionReader (Maybe Error)
checkPromotion mv = liftM (checkPromotes mv) (shouldPromote mv)

checkPromotes :: Move -> Bool -> Maybe Error
checkPromotes mv should =
    let promo = promotion mv
    in if should
       then flipMaybe PromotionRequired promo
       else promo >> Just IllegalPromotion



checkPromotedPawns :: PositionReader (Maybe Error)
checkPromotedPawns =
  justWhenM1 (anyM onLastRank =<< pawnSquares) PromotionRequired
  where pawnSquares = filterM pawnAt =<< friendlySquares


checkKingSafe :: PositionReader (Maybe Error)
checkKingSafe = do
  ks <- kingSquare
  case ks of
    Nothing -> return $ Just MissingKing
    Just s -> justWhenM1 (isAttacked s) KingCapturable


checkLegal :: PositionReader (Maybe Error)
checkLegal = firstError [checkKingSafe, checkPromotedPawns]


----------------------------------
-- Chess Utils
--

isLegalMove :: Move -> PositionReader Bool
isLegalMove mv = isRight <$> move mv

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
          couldMove src = canMoveNaive Move { source = src,
                                              destination = sq,
                                              promotion = Nothing }

canMoveNaive :: Move -> PositionReader Bool
canMoveNaive mv = isRight <$> moveNaive mv

friendlySquares :: PositionReader [Square]
friendlySquares = filterM friendlyAt =<< occupiedSquares

occupiedSquares :: PositionReader [Square]
occupiedSquares = liftM (keys. board) ask

isLegal :: PositionReader Bool
isLegal = liftM isJust checkLegal

onLastRank sq = do last <- lastRank
                   return $ rank sq == last

onInitialRank sq = do initial <- initialRank
                      return $ rank sq == initial


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
