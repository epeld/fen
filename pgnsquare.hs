{-#LANGUAGE NoMonomorphismRestriction #-}
module PGNSquare where
import Text.Parsec
import Data.Char (digitToInt)

import qualified Square

square = do
    f <- file
    r <- rank
    return $ Square.square' f r

file = oneOf "abcdefgh" <?> "file"
rank = fmap digitToInt (oneOf "12345678") <?> "rank"
