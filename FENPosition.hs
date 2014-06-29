module FENPosition where


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
encodeCastlingRights = sort. mapcat encode. Data.Set.toList

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


fenSquares = Square <$> files <*> reverse ranks
