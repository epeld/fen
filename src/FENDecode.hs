module FENDecode where
import Data.List.Split
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad
import Control.Applicative
import Text.Read
import Data.Char

import qualified Position
import Position (Position(..), Board)
import PositionProperties (Properties(..))
import Piece
import Castling
import Square
import FENEncode (fenSquares)

-- 
-- rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1
--

data Error = InvalidNumberOfComponents Int | 
             InvalidPieceCharacter Char | 
             InvalidNumberOfRows Int |
             InvalidTurn String | 
             InvalidPassant String | 
             InvalidMoveCount String | 
             InvalidCastlingRight Char
             deriving (Show, Eq)

initialFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

fen :: String -> Either Error Position
fen s = parts (words s)


parts :: [String] -> Either Error Position
parts (x : xs) = liftM2 Position (board x) (properties xs)
parts ps = invalidNumberOfComponents ps


properties :: [String] -> Either Error Properties
properties [b, t, c, p, h, f] = do
    trn <- turn t
    pss <- passant p
    fll <- moveCount f
    hlf <- moveCount h
    crs <- castlingRights c
    return (Properties trn pss fll hlf crs)

properties ps = invalidNumberOfComponents ps

invalidNumberOfComponents :: [a] -> Either Error b
invalidNumberOfComponents ps = Left $ InvalidNumberOfComponents (length ps)

castlingRights :: String -> Either Error (Set.Set CastlingRight)
castlingRights s = do
    let decode c = lookup c [ ('K', Castling White Kingside)
                            , ('Q', Castling White Queenside)
                            , ('k', Castling Black Queenside)
                            , ('q', Castling Black Queenside) ]

        castlingRight c = case decode c of
            Nothing -> Left (InvalidCastlingRight c)
            Just r -> Right r


    rs <- mapM castlingRight s
    return (Set.fromList rs)


moveCount :: String -> Either Error Int
moveCount s = case readMaybe s of
    Nothing -> Left (InvalidMoveCount s)
    Just x -> Right x


passant :: String -> Either Error (Maybe Square)
passant "-" = Right Nothing
passant s = case Square.square s of
    Nothing -> Left $ InvalidPassant s
    sq@(Just x) -> Right sq


turn :: String -> Either Error Color
turn "w" = Right White
turn "b" = Right Black
turn s  = Left (InvalidTurn s)


board :: String -> Either Error Board
board s = do

    let splitRows r = case length rows of
            8 -> Right rows
            x -> Left (InvalidNumberOfRows x)
            where
            rows = wordsBy (== '/') r


    rs <- splitRows s
    pcs <- mapM row rs

    let b = fromMaybeList (fenSquares `zip` concat pcs)
    
    return b
    


row :: String -> Either Error [Maybe Piece]
row s = do
    let decode c = if isDigit c 

                   -- is run length encoded whitespace
                   then nothings (digitToInt c)

                   -- is piece
                   else justOnePiece c


        justOnePiece c = do
            pc <- piece c
            return [Just pc]


        nothings c = Right (replicate c Nothing)


    parts <- mapM decode s
    return (concat parts)


piece :: Char -> Either Error Piece
piece c = case Piece <$> pt c <*> color c of
        Nothing -> Left (InvalidPieceCharacter c)
        Just p -> Right p

    where

    pt c = lookup (toLower c) [ ('p', Pawn)
                              , ('b', Officer Bishop)
                              , ('n', Officer Knight)
                              , ('r', Officer Rook)
                              , ('k', Officer King)
                              , ('q', Officer Queen) ]


    color c = lookup (isUpper c, isLower c) [ ((True, False), White)
                                            , ((False, True), Black) ]


fromMaybeList :: Ord a => [(a, Maybe b)] -> Map.Map a b
fromMaybeList = Map.mapMaybe id . Map.fromList
