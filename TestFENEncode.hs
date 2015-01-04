module TestFENEncode where
import qualified Data.Map as Map
import qualified Data.Set as Set
import Test.Hspec

import FENEncode as FEN
import Castling
import Piece
import Position
import qualified Square

-- rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1

square :: String -> Square.Square
square sq = case Square.square sq of
    Nothing -> error ("Invalid square " ++ sq)
    Just s -> s

emptyPosition :: Position
emptyPosition = Position { board = Map.empty,
                           turn = White,
                           passant = Nothing,
                           fullMoveCount = 1,
                           halfMoveCount = 0,
                           castlingRights = Set.empty }


main :: IO ()
main = hspec $ do
    describe "FENEncode.encode" $ do

        let pos = emptyPosition
            part :: Int -> String -> String
            part n s = words s !! n

        it "encodes a position into 6 parts" $ do
            length (words $ FEN.encode pos) `shouldBe` 6

        describe "the board-part" $ do
            let encoded :: Position -> String
                encoded = part 0. FEN.encode
            
            it "encodes a white knight at e4" $ do
                let _N = Piece (Officer Knight) White
                    e4 = square "e4"
                    p = pos { board = Map.singleton e4 _N}
                encoded p `shouldBe` "8/8/8/8/4N3/8/8/8"

            it "encodes a black knight at d5" $ do
                let n = Piece (Officer Knight) Black
                    d5 = square "d5"
                    p = pos { board = Map.singleton d5 n}
                encoded p `shouldBe` "8/8/8/3n4/8/8/8/8"

        describe "the turn-part" $ do
            let encoded :: Position -> String
                encoded = part 1. FEN.encode

            it "encodes white's turn as 'w'" $ do
                let p = pos { turn = White }
                encoded p `shouldBe` "w"

            it "encodes black's turn as 'b'" $ do
                let p = pos { turn = Black }
                encoded p `shouldBe` "b"


        describe "the castling rights-part" $ do
            let encoded :: Position -> String
                encoded = part 2. FEN.encode

            it "encodes whites queenside castling right as 'K'" $ do
                let _K = Castling White Kingside
                    p = pos { castlingRights = Set.singleton _K}
                encoded p `shouldBe` "K"

            it "encodes a lack of castling rights as '-'" $ do
                let p = pos { castlingRights = Set.empty }
                encoded p `shouldBe` "-"


        describe "the passant square-part" $ do
            let encoded :: Position -> String
                encoded = part 3. FEN.encode

            it "encodes a missing passant square as '-'" $ do
                let p = pos { passant = Nothing }
                encoded p `shouldBe` "-"

            it "encodes e4 as 'e4'" $ do
                let p = pos { passant = Square.square "e4" }
                encoded p `shouldBe` "e4"


        describe "the half move count-part" $ do
            let encoded :: Position -> String
                encoded = part 4. FEN.encode

            it "encodes move count 17 as '17'" $ do
                let p = pos { halfMoveCount = 17 }
                encoded p `shouldBe` "17"


        describe "the full move count-part" $ do
            let encoded :: Position -> String
                encoded = part 5. FEN.encode

            it "encodes move count 23 as '23'" $ do
                let p = pos { fullMoveCount = 23 }
                encoded p `shouldBe` "23"
