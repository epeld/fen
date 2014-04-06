module Types where
import Data.Set (Set)
import Data.Map (Map)
import Control.Monad.Reader (Reader)
import Control.Monad.Trans.Reader (ReaderT)

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

type PositionReader = Reader Position
type PositionReaderT = ReaderT Position
