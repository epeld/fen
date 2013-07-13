module Main where
import Control.Monad
import System.IO
import Text.Parsec

import Position
import ErrorMonad
import PGNParse
import PGNMove
import MoveLogic
import FEN
import FENEncode

main = processLines stdin stdout

processLines h o = do
    eof <- isEOF
    unless eof $ do
        processLine h o
        processLines h o

processLine h o = do
    s <- hGetLine h
    let r = parsePGNMoves s
    hPutStrLn o $
        case r of
            Right mvs -> show $ liftM encode $ applyMoves startingPosition mvs
            Left err -> show err

parsePGNMoves = Text.Parsec.parse (sepBy1 pgnMove space) "(stdin)"

applyMoves :: Position -> [PGNMove] -> ErrorMonad Position
applyMoves p [] = Right p
applyMoves p (mv:mvs) = p' >>= flip applyMoves mvs
    where p' = liftM positionAfter $ translate mv p
