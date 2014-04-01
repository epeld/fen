module Pgn where
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
                Takes 
                deriving (Show, Eq)

data SourceIndicator = File Char | 
                       Rank Int | 
                       Square Chess.Square
                       deriving (Show, Eq)

