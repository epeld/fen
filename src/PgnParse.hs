{-#LANGUAGE NoMonomorphismRestriction #-}
module PgnParse where

import PartialDescription as Partial

--pgnMove = choice [try pawnMove, try officerMove, castles]

pawnMove :: Parser Move
pawnMove = PawnMove <$> descs <*> optionMaybe promotion
    where
    descs = choice [try longPawnMoveDesc, shortPawnMoveDesc]

officerMove :: Parser Move
officerMove = OfficerMove <$> officer <*> descs
    where
    descs = choice [try longOfficerMoveDesc, shortOfficerMoveDesc]

--castlesKingside = string "O-O" >> return (Pgn.Castles Kingside)
--castlesQueenside = string "O-O-O" >> return (Pgn.Castles Queenside)

--castles = try castlesQueenside <|> castlesKingside


shortOfficerMoveDesc :: Parser Partial.Description
shortOfficerMoveDesc = do
    mt <- moveType
    dst <- square
    return $ Partial.Description dst mt Nothing


longOfficerMoveDesc :: Parser Partial.Description
longOfficerMoveDesc = do
    src <- officerSource
    mt <- moveType
    dst <- square
        return $ Partial.Description dst mt src


-- E.g "exd4"
longPawnMoveDesc :: Parser Partial.Description
longPawnMoveDesc = do
    src <- partialSquare -- TODO, really only file or rank
    mt <- moveType
    dst <- square
    return $ Description dst mt src


--  E.g "e4"
shortPawnMoveDesc :: Parser Partial.Description
shortPawnMoveDesc = do
    dst <- square
    return $ Description dst Moves Nothing
    

moveType :: Parser MoveType
moveType = Moves `option` captures
    where
    captures = char 'x' >> return Captures


promotion :: Parser Officer
promotion = do
    char '='
    rook <|> knight <|> bishop <|> queen


rook :: Parser OfficerType
rook = char 'R' >> return Rook


knight :: Parser OfficerType
knight = char 'N' >> return Knight


bishop :: Parser OfficerType
bishop = char 'B' >> return Bishop


queen :: Parser OfficerType
queen = char 'Q' >> return Queen


king :: Parser OfficerType
king = char 'K' >> return King


officer :: Parser Officer
officer = king <|> queen <|> rook <|> bishop <|> knight


officerSource :: Parser PartialSquare
officerSource = choice [rankPartial, filePartial, squarePartial]


rankPartial :: Parser PartialSquare
rankPartial = choice ranks
    ranks = forM [1..9] $ \x -> char (intToDigit x) >> Partial.Rank x


filePartial :: Parser PartialSquare
filePartial = choice files
    files = forM ['a'..'h'] $ \x -> char x >> Partial.File x


squarePartial :: Parser PartialSquare
squarePartial = Whole <$> square
