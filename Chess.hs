


move :: Position -> Square -> Square
     -> Either Error Position

move pos source dest = let newPos = moveNaive pos source dest
                           errs = checkSource pos source <|> 
                                  checkLegal newPos
                        in if isNothing errs
                           then Right newPos
                           else Left KingCapturable
                            
-- Naive move: move disregarding king safety
moveNaive :: Position -> Square -> Square
          -> Either Error Position
        
moveNaive pos source dest = let t = pieceTypeAt pos source
                             in if t == Pawn
                                then moveNaivePawn pos source dest
                                else moveNaiveOfficer pos source dest

moveNaivePawn pos source dest = if capturableAt pos dest
                                then naivePawnCapture pos source dest
                                else naivePawnMove pos source dest

naivePawnTakes pos source dest = let errs = checkAdjacent source dest <|>
                                            checkOneUp pos source dest
                                     newPos = performMove pos source dest
                                  in maybeEither errs newPos

-- peformMove is the function that actually performs the work;
-- moves the piece from source to dest and updates the position
performMove pos source dest = pos { turn = calcTurn pos,
                                    board = calcBoard pos source dest,
                                    fullMoveNr = calcFullMoveNr pos,
                                    halfMoveNr = calcHalfMoveNr pos source dest,
                                    passant = calcPassant pos source dest,
                                    castling = calcCastling pos source dest }

maybeEither :: Maybe a -> b -> Either a b
maybeEither ma b = maybe (Right b) Left ma

checkOneUp = checkAhead 1
checkAhead :: Int -> Position -> Square -> Square
           -> Maybe Error

checkAhead n pos source dest = let r = rank source
                                   r2 = rank dest
                                in case turn pos of
                                      White -> n == r2 - r
                                      Black -> n == r - r2

checkAdjacent :: Square -> Square -> Maybe Error
checkAdjacent source dest = let f1 = file source
                                f2 = file dest
                             in boolMaybe FilesNotAdjacent (1 /= abs (f1 - f2))

checkSource :: Position -> Square -> Maybe Error
checkSource pos source = let pclr = colorAt pos source
                          in maybe (Just NoPiece) checkColorsMatch

checkColorsMatch :: Position -> Color -> Maybe Error
checkColorsMatch = boolMaybe WrongColor. (pieceColor/=). turn

checkLegal :: Position -> Maybe Error
checkLegal = boolMaybe KingCapturable. legal

legal :: Position -> Bool
legal pos = True

boolMaybe :: a -> Bool -> Maybe a
boolMaybe a b = if b then Just a else Nothing
