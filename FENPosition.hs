module FENPosition where
import Control.Monad ((>>=))
import Control.Applicative ((<*>), (<$>), pure)

import Data.Char (isDigit)
import Data.List (groupBy, intersperse, sort)
import Data.List.Split (splitOn, chunksOf)
import Data.Maybe (isNothing, catMaybes)
import Castling (CastlingRight)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Piece (Piece)
import FEN (FEN, encode, decode, decodeChar)
import Square (Square(Square), files, ranks)
import Position
import Utils

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

decodeCastlingRights :: String -> Maybe (Set.Set CastlingRight)
decodeCastlingRights s = let r = sequence $ map (\x -> decode [x]) s
                         in fmap Set.fromList r

encodeCastlingRights :: Set.Set CastlingRight -> String
encodeCastlingRights rs = sort $ Set.toList rs >>= encode

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
encodeBoard b = let pieces = map (flip Map.lookup b) fenSquares
                    rows = chunksOf 8 pieces
                in concat $ intersperse "/" $ map encodeRow rows

decodeBoard :: String -> Maybe Board
decodeBoard s = do
    rows <- fmap concat $ sequence $ map decodeRow $ splitOn "/" s

    let assocs :: [Maybe (Square, Piece)]
        assocs = zipWith (\a b -> fmap ((,) a) b) fenSquares rows

    Just $ Map.fromList $ catMaybes $ assocs


type Row = [Maybe Piece]

encodeRow :: Row -> String
encodeRow r = let enc [Just p] = encode p
                  enc nothings = show (length nothings)
              in groupNothings r >>= enc

groupNothings :: Eq a => [Maybe a] -> [[Maybe a]]
groupNothings =
  let areNothing a b = a == b && isNothing a
  in groupBy areNothing

decodeRow :: String -> Maybe Row
decodeRow s = let dec :: Char -> Maybe [Maybe Piece]
                  dec x = if isDigit x
                          then Just $ digitToNothings x
                          else fmap (\x -> [Just x]) (decodeChar x)
                  pieces = sequence $ map dec s
               in fmap concat pieces

digitToNothings :: Char -> [Maybe a]
digitToNothings c = let n = read [c]
                    in replicate n Nothing

fenSquares = Square <$> files <*> reverse ranks
