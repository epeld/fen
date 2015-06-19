module FENDecode where
import Prelude ()
import Data.Either
import Data.Char
import Data.Bool
import Data.Function
import Data.Maybe
import Data.List
import Data.Int
import Data.Eq
import Data.String
import Data.Monoid
import Data.List.Split
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad
import Control.Applicative
import Text.Read
import Text.Show

import Position
import Piece
import Castling
import Square
import FENEncode

-- 
-- rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1
--

data Error = InvalidNumberOfComponents Int | InvalidPieceCharacter Char | InvalidNumberOfRows Int |
             InvalidTurn String | InvalidPassant String | InvalidMoveCount String | InvalidCastlingRight Char
             deriving (Show, Eq)

initialFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

decode :: String -> Either Error Position
decode s = decodeParts (words s)


decodeParts :: [String] -> Either Error Position
decodeParts [b, t, c, p, h, f] = do
    brd <- decodeBoard b
    trn <- decodeTurn t
    pss <- decodePassant p
    fll <- decodeMoveCount f
    hlf <- decodeMoveCount h
    crs <- decodeCastlingRights c
    return $ Position brd trn pss fll hlf crs

decodeParts parts = Left $ InvalidNumberOfComponents (length parts)

decodeCastlingRights :: String -> Either Error (Set.Set CastlingRight)
decodeCastlingRights s = Set.fromList `liftM` forM s decode
    where
    decode :: Char -> Either Error CastlingRight
    decode 'K' = Right $ Castling White Kingside
    decode 'Q' = Right $ Castling White Queenside 
    decode 'k' = Right $ Castling Black Kingside
    decode 'q' = Right $ Castling Black Queenside
    decode c = Left $ InvalidCastlingRight c


decodeMoveCount :: String -> Either Error Int
decodeMoveCount s = e (readMaybe s)
    where
    e Nothing = Left $ InvalidMoveCount s
    e (Just x) = Right x


decodePassant :: String -> Either Error (Maybe Square)
decodePassant "-" = Right Nothing
decodePassant sq = e (Square.square sq)
    where
    e Nothing = Left $ InvalidPassant sq
    e x = Right x


decodeTurn :: String -> Either Error Color
decodeTurn "w" = Right White
decodeTurn "b" = Right Black
decodeTurn s  = Left (InvalidTurn s)


decodeBoard :: String -> Either Error Board
decodeBoard s = do
    rows <- splitRows s
    pcs <- forM rows decodeRow 

    let assocs :: [(Square, Maybe Piece)]
        assocs = zip fenSquares (mconcat pcs)

        f :: (Square, Maybe Piece) -> Maybe (Square, Piece)
        f (_, Nothing) = Nothing
        f (sq, Just x) = Just (sq, x)
    
    return $ Map.fromList $ mapMaybe f assocs
    


decodeRow :: String -> Either Error [Maybe Piece]
decodeRow s = mconcat `liftM` mapM dec s
    where 
    dec :: Char -> Either Error [Maybe Piece]
    dec c = if isDigit c 

            -- is run length encoded whitespace
            then Right $ replicate (digitToInt c) Nothing

            -- is piece
            else (replicate 1. Just) `fmap` (decodePiece c)


decodePiece :: Char -> Either Error Piece
decodePiece c = e (Piece <$> pt (toLower c) <*> colorFromCase c)
    where
    e Nothing = Left (InvalidPieceCharacter c)
    e (Just p) = Right p






pt :: Char -> Maybe PieceType
pt 'p' = Just $ Pawn
pt 'b' = Just $ Officer Bishop
pt 'r' = Just $ Officer Rook
pt 'n' = Just $ Officer Knight
pt 'k' = Just $ Officer King
pt 'q' = Just $ Officer Queen
pt _ = Nothing


colorFromCase :: Char -> Maybe Color
colorFromCase c = 
    case (isUpper c, isLower c) of
        (True, False) -> Just White
        (False, True) -> Just Black
        _ -> Nothing


splitRows :: String ->  Either Error [String]
splitRows s = verify (length rows)
    where
    rows = wordsBy (== '/') s
    verify 8 = Right rows
    verify x = Left (InvalidNumberOfRows x)
