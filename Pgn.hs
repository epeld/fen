module Pgn where
import Control.Monad ((=<<), filterM, liftM)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Identity (Identity)
import Control.Monad.Reader (Reader, ask)
import Control.Applicative ((<$>), (<*>), pure)
import qualified Chess
import qualified Types as Chess
import Utils (everyM, anyM)

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


resolve :: Pgn.Move -> Chess.PositionReader (Either Error Chess.Move)
resolve mv = move <$> implied mv

move :: [Chess.Move] -> Either Error Chess.Move
move [] = Left NoCandidate
move [mv] = Right mv
move moves = Left $ AmbigousMove moves

implied :: Pgn.Move -> Chess.PositionReader [Chess.Move]
implied mv = filterM Chess.isLegal =<< moves mv

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
