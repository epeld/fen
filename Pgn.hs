module Pgn where
import Control.Monad ((=<<), filterM, liftM, liftM2)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Identity (Identity)
import Control.Monad.Reader (Reader, ask)
import Control.Applicative ((<$>), (<*>), pure)
import Data.Maybe (fromJust)
import qualified Chess
import qualified Types as Chess
import Utils (everyM, anyM, ifM1)

data Move = PieceMove {
                pieceType :: Chess.PieceType,
                source :: Maybe SourceIndicator,
                moveType :: MoveType,
                destination :: Chess.Square,
                promotion :: Maybe Chess.OfficerType } |
            Castles Chess.Side
            deriving (Show, Eq)

data MoveType = Moves |
                Captures
                deriving (Show, Eq)

data SourceIndicator = File Char |
                       Rank Int |
                       Square Chess.Square
                       deriving (Show, Eq)

data Error = CaptureNotPossible |
             CaptureRequired |
             AmbigousMove [Chess.Move] | -- TODO use Pgn.Move here
             NoCandidate
             deriving (Show, Eq)

move :: Move -> Chess.PositionReader (Either Error Chess.Position)
move mv = do
  r <- resolve mv
  case r of
    Left err -> return $ Left err
    Right cmv -> do p <- Chess.move cmv
                    return $ either (error. show) Right p

encode :: Chess.Move -> Chess.PositionReader (Either Chess.Error Pgn.Move)
encode mv = do
  legalPos <- Chess.move mv
  pgn <- encodeLegal mv
  return (legalPos >> Right pgn)

encodeLegal :: Chess.Move -> Chess.PositionReader Pgn.Move
encodeLegal mv =
  PieceMove <$> liftM fromJust (Chess.pieceTypeAt src)
            <*> liftM (distinguish src) (findSimilarSources mv)
            <*> inferMoveType mv
            <*> pure (Chess.destination mv)
            <*> pure (Chess.promotion mv)
  where src = Chess.source mv

findSimilarSources :: Chess.Move -> Chess.PositionReader [Chess.Square]
findSimilarSources mv = do
  pt <- liftM fromJust $ Chess.pieceTypeAt (Chess.source mv)
  let canMove sq = Chess.isLegalMove $
                   Chess.Move sq (Chess.destination mv) (Chess.promotion mv)
  filterM canMove =<< findFriendly pt


-- TODO move this to Chess module
findFriendly :: Chess.PieceType -> Chess.PositionReader [Chess.Square]
findFriendly pt = filterM matchingPieceType =<< Chess.friendlySquares
  where matchingPieceType sq = liftM (Just pt ==) (Chess.pieceTypeAt sq)

distinguish :: Chess.Square -> [Chess.Square] -> Maybe SourceIndicator
distinguish sq xs = if elem sq xs
                    then distinguish' sq xs
                    else error "Fatal error in chess logic" -- sanity check

distinguish' :: Chess.Square -> [Chess.Square] -> Maybe SourceIndicator
distinguish' sq [x] = Nothing
distinguish' sq sqs =
  let r = Chess.rank sq
      f = Chess.file sq
      ranks = map Chess.rank sqs
      files = map Chess.file sqs in
  case (any (f ==) files, any (r ==) ranks) of
    (True, True) -> Just $ Square sq
    (True, False) -> Just $ Rank r
    _ -> Just $ File f

-- TODO move this to Chess module?
inferMoveType :: Chess.Move -> Chess.PositionReader MoveType
inferMoveType mv = ifM1 (Chess.isCapture mv) Captures Moves

resolve :: Pgn.Move -> Chess.PositionReader (Either Error Chess.Move)
resolve mv = onlyMove <$> implied mv

onlyMove :: [Chess.Move] -> Either Error Chess.Move
onlyMove [] = Left NoCandidate
onlyMove [mv] = Right mv
onlyMove moves = Left $ AmbigousMove moves

implied :: Pgn.Move -> Chess.PositionReader [Chess.Move]
implied mv = filterM Chess.isLegalMove =<< moves mv

moves :: Pgn.Move -> Chess.PositionReader [Chess.Move]
moves mv =  map makeMove <$> sources mv
    where makeMove s = Chess.Move{ Chess.destination = destination mv,
                                   Chess.promotion = promotion mv,
                                   Chess.source = s }

sources :: Pgn.Move -> Chess.PositionReader [Chess.Square]
sources mv = Chess.friendlySquares >>= candidates mv

candidates :: Pgn.Move -> [Chess.Square] -> Chess.PositionReader [Chess.Square]
candidates mv = filterM (isCandidate mv)

isCandidate :: Move -> Chess.Square -> Chess.PositionReader Bool
isCandidate mv sq = everyM [ matchesSource mv sq, matchesPieceType mv sq ]


matchesPieceType :: Move -> Chess.Square -> Chess.PositionReader Bool
matchesPieceType mv sq = liftM (Just (pieceType mv) ==) (Chess.pieceTypeAt sq)


matchesSource :: Move -> Chess.Square -> Chess.PositionReader Bool
matchesSource mv sq = return $ matches (source mv)
    where matches Nothing = True
          matches (Just (Square s)) = s == sq
          matches (Just (File f)) = f == Chess.file sq
          matches (Just (Rank r)) = r == Chess.rank sq
