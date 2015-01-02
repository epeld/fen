module FENDecode where
import Prelude ()
import Data.Either
import Data.Char
import Data.List.Split

import Position

-- 
-- rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1
--

decode :: String -> Either String Position
decode s = 
    let parts = words s
    in case parts of 
        6 -> decodeParts parts
        _ -> s

colorFromCase :: Char -> Color
colorFromCase c = 
    if isUpperCase c
    then White
    else Black


decodeBoard :: String -> Either String Board
decodeBoard s = do
    rows <- splitRows s
    bla decodeBoardRow rows


decodeBoardRow :: String -> Either String [Maybe Piece]
decodeBoardRow s = concatMap dec s
    where   dec c = if isDigit c 
                    then replicate (digitToInt c) Nothing
                    -- TODO all wrong.. needs fixing
                    else flip Piece (colorFromCase c) <$> pt (toLower c)

pt :: Char -> Maybe PieceType
pt 'p' = Just $ Pawn
pt 'b' = Just $ Officer Bishop
pt 'r' = Just $ Officer Rook
pt 'n' = Just $ Officer Knight
pt 'k' = Just $ Officer King
pt 'q' = Just $ Officer Queen
pt _ = Nothing


splitRows :: String ->  Either String [String]
splitRows s = 
    let rows = wordsBy (== '/') in
    case rows of
        8 -> Right rows
        _ -> s
