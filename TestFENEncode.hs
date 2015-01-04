module TestFENEncode where
import qualified Data.Map as Map
import qualified Data.Set as Set
import Test.Hspec

import FENEncode as FEN
import Castling
import Piece
import Position

-- rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1

main = hspec $ do
    describe "encode" $ do

        let pos = Position { board = Map.empty,
                             turn = White,
                             passant = Nothing,
                             fullMoveCount = 0,
                             halfMoveCount = 1,
                             castlingRights = Set.empty }
            part :: Int -> String -> String
            part n s = words s !! n


        it "encodes a position into 6 parts" $ do
            length (words $ FEN.encode pos) `shouldBe` 6

        it "encodes white's turn as 'w'" $ do
            let p = pos { turn = White }
            part 1 (FEN.encode p) `shouldBe` "w"

        it "encodes black's turn as 'b'" $ do
            let p = pos { turn = Black }
            part 1 (FEN.encode p) `shouldBe` "b"

        it "encodes whites queenside castling right as 'K'" $ do
            let _K = Castling White Kingside
                p = pos { castlingRights = Set.singleton _K}
            part 2 (FEN.encode p) `shouldBe` "K"

        it "encodes a lack of castling rights as '-'" $ do
            let p = pos { castlingRights = Set.empty }
            part 2 (FEN.encode p) `shouldBe` "-"

        it "encodes a missing passant square as '-'" $ do
            let p = pos { passant = Nothing }
            part 3 (FEN.encode p) `shouldBe` "-"
