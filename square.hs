{-#LANGUAGE NoMonomorphismRestriction #-}
module Square where 
import Control.Monad
import Control.Monad.Trans.Cont
import Control.Applicative
import Data.List
import Data.Ix
import Data.Maybe
import Data.Char
import Piece
import Text.Parsec

instance Enum (File) where
    toEnum i = let i' = fromEnum 'a' + i 
                in File (toEnum i')
    fromEnum (File c) = fromEnum c - fromEnum 'a'
instance Enum (Rank) where
    toEnum i = let i' = i + 1
                in Rank (toEnum i')
    fromEnum (Rank c) = fromEnum c - 1
instance Enum (Square) where
    toEnum i = Square (toEnum $ mod i 8 ) (toEnum $ div i 8)
    fromEnum (Square f r) = 8 * (fromEnum r) + fromEnum f
m !!! s = m !! fromEnum s

data File = File Char deriving (Show, Eq, Ix, Ord)
data Rank = Rank Int deriving (Show, Eq, Ix, Ord)
data Square = Square {
    file :: File,
    rank :: Rank
    } deriving (Show, Eq, Ix, Ord)
files = [File 'a'..File 'h']
ranks = [Rank 1..Rank 8]

validFile f = f `elem` files
validRank r = r `elem` ranks
validSquare (Square f r) = validFile f && validRank r

allSquares :: [Square]
allSquares = map toEnum [0..63]

parseSquare s = parse square s s

square = do
    f <- oneOf ['a'..'h'] 
    r <- digitToInt <$> oneOf ['1'..'8']
    return $ Square (File f) (Rank r)

squareToString (Square (File f) (Rank r)) =
    f : show r

offset (v,h) (Square f r) = 
    let f' = toEnum $ fromEnum f + h
        r' = toEnum $ fromEnum r + v
     in
        return $ Square f' r'

up v = offset (v,0)
down v = up (-v)
right h = offset (0,h)
left h = left (-h)

fromSquare s c = runCont (c s) id

sequence s c =
    let f = flip fromSquare c
     in
        takeWhile validSquare (iterate f s)

sequence' s c = drop 1 $ sequence s c

relUp v c =
    case c of
        White -> up v
        Black -> down v

