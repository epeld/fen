{-#LANGUAGE NoMonomorphismRestriction #-}
module Square(
    Square,
    square,
    file,
    rank,
    fileLetters,
    rankNumbers,
    files,
    ranks) where 
import Control.Applicative
import Control.Monad
import Data.List
import Data.Ix
import Data.Maybe
import Data.Char

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
data Square = Square File Rank deriving (Show, Eq, Ix, Ord)

fileLetters = ['a'..'h']
rankNumbers = [1..8]
files = File <$> fileLetters
ranks = Rank <$> rankNumbers

file f = File <$> find (f==) fileLetters
rank r = Rank <$> find (r==) rankNumbers
square f r = liftM2 Square (file f) (rank r)
