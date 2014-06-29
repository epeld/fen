module FEN where

class FEN f where
    encode :: f -> String
    decode :: String -> Maybe f

decodeChar :: FEN f => Char -> Maybe f
decodeChar c = decode [c]
