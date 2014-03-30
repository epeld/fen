module Fen where
import Prelude hiding (lookup)
import Data.List (groupBy, intersperse)
import Data.Maybe
import Data.Char ( toUpper, 
                   toLower, 
                   isUpper, 
                   isDigit, 
                   intToDigit,
                   isDigit,
                   digitToInt )
import Data.Map (fromList, lookup, unions)
import qualified Data.Set (toList, fromList, Set(..))
import Data.List.Split (splitOn, chunksOf)
import Control.Applicative ((<*>), (<$>), pure)
import Chess ( Position(..),
               Board,
               Square(..),
               Color(..),
               Side(..), 
               Piece(..), 
               PieceType(..),
               OfficerType(..),
               CastlingRight(..),
               newRank, newFile )

class FEN f where
    encode :: f -> String
    decode :: String -> Maybe f

--  rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1
instance FEN Position where
    encode p = unwords [ encodeBoard (board p),
                         encode (turn p),
                         encodeCastlingRights (castling p),
                         encodePassant (passant p),
                         show (halfMoveNr p),
                         show (fullMoveNr p) ]

    decode s = let parts = words s
                in Position <$> decodeBoard (parts !! 0)
                            <*> decodePassant (parts !! 3)
                            <*> decodeInt (parts !! 4)
                            <*> decodeInt (parts !! 5)
                            <*> decode (parts !! 1)
                            <*> decodeCastlingRights (parts !! 2)

decodeCastlingRights :: String -> Maybe (Data.Set.Set CastlingRight)
decodeCastlingRights s = let r = sequence $ map (\x -> decode [x]) s
                         in fmap Data.Set.fromList r

encodeCastlingRights :: Data.Set.Set CastlingRight -> String
encodeCastlingRights = mapcat encode. Data.Set.toList

encodePassant :: Maybe Square -> String
encodePassant = maybe "-" encode

decodePassant :: String -> Maybe (Maybe Square)
decodePassant s = if s == "-"
                  then Just Nothing
                  else fmap Just (decode s)

decodeInt :: String -> Maybe Int
decodeInt s = if all isDigit s
              then Just $ read s
              else Nothing

encodeBoard :: Board -> String
encodeBoard b = let pieces = map (flip lookup b) fenSquares
                    rows = chunksOf 8 pieces
                in concat $ intersperse "/" (map encodeRow rows)
                
decodeBoard :: String -> Maybe Board
decodeBoard s = do
    rows <- fmap concat $ sequence $ map decodeRow $ splitOn "/" s

    let assocs :: [Maybe (Square, Piece)] 
        assocs = zipWith (\a b -> fmap ((,) a) b) fenSquares rows

    Just $ fromList $ catMaybes $ assocs


type Row = [Maybe Piece]

encodeRow :: Row -> String
encodeRow = let rle = groupBy areNothing
                enc (Just p : []) = encode p
                enc nothings = show (length nothings)
                areNothing a b = a == b && isNothing a
             in mapcat enc. rle

decodeRow :: String -> Maybe Row
decodeRow s = let dec :: Char -> Maybe [Maybe Piece]
                  dec x = if isDigit x
                          then Just $ replicate (read [x]) Nothing
                          else fmap (\x -> [Just x]) (decode [x])
                  pieces = sequence $ map dec s
               in fmap concat pieces

instance FEN Square where
    encode (Square f r) = [f, intToDigit r]

    decode (f:r:[]) = let r' = digitToInt r
                       in Square <$> newFile f <*> newRank r'
    decode _ = Nothing


instance FEN Color where
    encode White = "w"
    encode Black = "b"

    decode "w" = Just White
    decode "b" = Just Black
    decode _ = Nothing

instance FEN CastlingRight where
    encode (Castling Kingside White) = "K"
    encode (Castling Queenside White) = "Q"
    encode (Castling Kingside Black) = "k"
    encode (Castling Queenside Black) = "q"

    decode "K" = Just $ Castling Kingside White
    decode "Q" = Just $ Castling Queenside White
    decode "k" = Just $ Castling Kingside Black 
    decode "q" = Just $ Castling Queenside Black
    decode _ = Nothing

instance FEN Piece where
    encode p = let pt = encode (pieceType p)
                in case color p of
                       White -> map toUpper pt
                       Black -> map toLower pt

    decode s@(x:[]) = let c = if isUpper x then White else Black
                       in Piece <$> decode s <*> pure c
    decode _ = Nothing

instance FEN PieceType where
    encode Pawn = "P"
    encode (Officer o) = encode o

    decode "P" = Just Pawn
    decode "p" = Just Pawn
    decode s = fmap Officer $ decode s

instance FEN OfficerType where
    encode Knight = "N"
    encode Bishop = "B"
    encode Rook = "R"
    encode King = "K"
    encode Queen = "Q"

    decode "N" = Just Knight
    decode "n" = Just Knight

    decode "B" = Just Bishop
    decode "b" = Just Bishop

    decode "R" = Just Rook
    decode "r" = Just Rook

    decode "K" = Just King
    decode "k" = Just King

    decode "Q" = Just Queen
    decode "q" = Just Queen

    decode _ = Nothing

mapcat f xs = concat $ map f xs

fenSquares = Square <$> files <*> reverse ranks

files = ['a', 'b'..'h']
ranks = [8,7..1]
