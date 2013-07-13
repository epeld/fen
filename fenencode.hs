module FENEncode where
import Data.List (intersperse, groupBy)
import Data.Maybe (isNothing)

import FEN (fenSquares)
import Position (readSquare)
import Piece (Piece, pieceToChar)

encode p = fenRLEEncodePieces p ++ ' ' : fenEncodeProperties p

fenEncodeProperties p = "TODO"

fenRLEEncodePieces p = concat $ intersperse "/" $ 
    map fenRLEEncodeRow rows
    where rows = every 8 [readSquare p s | s <- fenSquares]

fenRLEEncodeRow r = concat $ map fenRLEEncodeGroupedRow grouped
    where grouped = groupBy areNothing r

fenRLEEncodeGroupedRow :: [Maybe Piece] -> String
fenRLEEncodeGroupedRow [] = ""
fenRLEEncodeGroupedRow l@(c:cs) = case c of
    Nothing -> show $ length l
    Just p -> pieceToChar p : fenRLEEncodeRow cs


maybeEncodePiece = maybe Nothing (Just. pieceToChar)

areNothing a b = isNothing a && isNothing b

every :: Int -> [a] -> [[a]]
every l s = takeWhile (not. null) $
    map (\x -> slice x l s) offsets
    where offsets = [x*l | x <- [0..]]

slice o l s = take l $ drop o $ s
