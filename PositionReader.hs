module PositionReader where

type PositionReader = Reader Position
type PositionReaderT = ReaderT Position

isPassantSquare :: Square -> PositionReader Bool
isPassantSquare sq = liftM ((Just sq ==). passant) ask

isForward :: Int -> Square -> Square -> PositionReader Bool
isForward n src dst = liftM (Just dst ==) (forwardN n src)


isEmpty :: Square -> PositionReader Bool
isEmpty sq = liftM isNothing (pieceTypeAt sq)

pawnAt :: Square -> PositionReader Bool
pawnAt sq = liftM (Just Pawn ==) (pieceTypeAt sq)

kingAt :: Square -> PositionReader Bool
kingAt sq = liftM (Just (Officer King) ==) (pieceTypeAt sq)

enemyAt :: Square -> PositionReader Bool
enemyAt sq = do
  clr <- colorAt sq
  clr2 <- enemyColor
  return $ clr == Just clr2

friendlyAt :: Square -> PositionReader Bool
friendlyAt sq = do
  clr <- colorAt sq
  clr2 <- liftM turn ask
  return $ clr == Just clr2

pieceAt :: Square -> PositionReader (Maybe Piece)
pieceAt sq = do
  pos <- ask
  return $ lookup sq (board pos)

pieceTypeAt :: Square -> PositionReader (Maybe PieceType)
pieceTypeAt sq = do pc <- pieceAt sq
                    return $ liftM pieceType pc

colorAt :: Square -> PositionReader (Maybe Color)
colorAt sq = do pc <- pieceAt sq
                return $ fmap color pc

findSquare :: (Square -> PositionReader Bool) -> PositionReader (Maybe Square)
findSquare pred = fmap safeHead (filterM pred =<< occupiedSquares)

enemyColor :: PositionReader Color
enemyColor = liftM Position.enemyColor

initialRank :: PositionReader Int
initialRank = liftM Position.initialRank

lastRank :: PositionReader Int
lastRank = liftM Position.lastRank
