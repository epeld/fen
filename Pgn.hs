module Pgn where
import Control.Monad ((=<<), filterM, liftM)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Identity (Identity)
import Control.Monad.Reader (Reader, ask)
import Control.Applicative ((<$>), (<*>), pure)
import qualified Chess

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
             AmbigousMove [Chess.Move] |
             NoCandidate
             deriving (Show, Eq)


type PositionReader = Reader Chess.Position 
type PositionReaderT = ReaderT Chess.Position 

legal :: Chess.Move -> PositionReader Bool
legal mv = do
    p <- ask
    return (either (const False) (const True) $ Chess.move p mv)

resolve :: Pgn.Move -> PositionReader (Either Error Chess.Move)
resolve mv = move <$> implied mv 

move :: [Chess.Move] -> Either Error Chess.Move
move [] = Left NoCandidate
move [mv] = Right mv
move moves = Left $ AmbigousMove moves

implied :: Pgn.Move -> PositionReader [Chess.Move]
implied mv = filterM legal =<< moves mv

moves :: Pgn.Move -> PositionReader [Chess.Move]
moves mv =  map makeMove <$> sources mv
    where makeMove s = Chess.Move{ Chess.destination = destination mv,
                                   Chess.promotion = promotion mv,
                                   Chess.source = s }

sources :: Pgn.Move -> PositionReader [Chess.Square]
sources mv = friendlySquares >>= candidates mv

candidates :: Pgn.Move -> [Chess.Square] -> PositionReader [Chess.Square]
candidates mv = filterM (isCandidate mv)

isCandidate :: Move -> Chess.Square -> PositionReader Bool
isCandidate mv sq = everyM [ matchesSource mv sq, matchesPieceType mv sq ]


matchesPieceType :: Move -> Chess.Square -> PositionReader Bool
matchesPieceType mv sq = do
    pt <- pieceTypeAt sq
    return $ pt == Just (pieceType mv)

matchesSource :: Move -> Chess.Square -> PositionReader Bool
matchesSource mv sq = return $ matches (source mv)
    where matches Nothing = True
          matches (Just (Square s)) = s == sq
          matches (Just (File f)) = f == Chess.file sq
          matches (Just (Rank r)) = r == Chess.rank sq

everyM :: Monad m => [m Bool] -> m Bool
everyM = liftM every. sequence

every :: [Bool] -> Bool
every = all id

friendlySquares :: PositionReader [Chess.Square]
friendlySquares = fmap Chess.friendlySquares ask

pieceTypeAt :: Chess.Square -> PositionReader (Maybe Chess.PieceType)
pieceTypeAt sq = Chess.pieceTypeAt <$> ask <*> pure sq


safeHead [] = Nothing
safeHead x = Just $ head x

flipMaybe a b = case a of
    Nothing -> Just b
    Just _ -> Nothing

maybeError :: Maybe a -> b -> Either a b
maybeError a b = maybe (Right b) Left a
