module Fen where

attempt = maybe Nothing

decode :: String -> Maybe Position
decode fen =
  let parts = words fen
   in papa


newtype BoardChar = Either Piece Int

boardChar :: Char -> Maybe BoardChar
boardChar c = if isDigit c
  then Just $ Left $ read c
  else attempt Right $ piece c
