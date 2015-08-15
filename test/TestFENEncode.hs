module TestFENEncode where
import Control.Lens

import qualified Data.Map as Map
import qualified Data.Set as Set
import Test.Hspec

import qualified FENEncode as FEN
import qualified Square
import Castling
import Piece
import Position
import PositionProperties (Properties(..))

-- rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1

square :: String -> Square.Square
square = Square.unsafe

emptyPosition :: Position
emptyPosition = Position { _board = Map.empty, _properties = props }
    where
    props = Properties { _turn = White
                       , _passant = Nothing
                       , _fullMoveCount = 1
                       , _halfMoveCount = 0
                       , _castlingRights = Set.empty }


test :: IO ()
test = hspec $ do
    describe "FENEncode.encode" $ do

        let pos = emptyPosition
            part :: Int -> String -> String
            part n s = words s !! n


        it "encodes a position into 6 parts" $ do
            length (words $ FEN.fen pos) `shouldBe` 6


        describe "the board-part" $ do
            let encoded = part 0. FEN.fen
            
            it "encodes a white knight at e4" $ do
                let _N = Piece (Officer Knight) White
                    e4 = square "e4"
                    p = pos & board .~ Map.singleton e4 _N

                encoded p `shouldBe` "8/8/8/8/4N3/8/8/8"


            it "encodes a black knight at d5" $ do
                let n = Piece (Officer Knight) Black
                    d5 = square "d5"
                    p = pos & board .~ Map.singleton d5 n

                encoded p `shouldBe` "8/8/8/3n4/8/8/8/8"


        describe "the turn-part" $ do
            let encoded = part 1. FEN.fen

            it "encodes white's turn as 'w'" $ do
                let p = pos & turn .~ White

                encoded p `shouldBe` "w"


            it "encodes black's turn as 'b'" $ do
                let p = pos & turn .~ Black

                encoded p `shouldBe` "b"


        describe "the castling rights-part" $ do
            let encoded = part 2. FEN.fen

            it "encodes whites queenside castling right as 'K'" $ do
                let _K = Castling White Kingside
                    p = pos & castlingRights .~ Set.singleton _K

                encoded p `shouldBe` "K"


            it "encodes a lack of castling rights as '-'" $ do
                let p = pos & castlingRights .~ Set.empty

                encoded p `shouldBe` "-"


        describe "the passant square-part" $ do
            let encoded = part 3. FEN.fen

            it "encodes a missing passant square as '-'" $ do
                let p = pos & passant .~ Nothing

                encoded p `shouldBe` "-"


            it "encodes e4 as 'e4'" $ do
                let p = pos & passant .~ Square.square "e4"

                encoded p `shouldBe` "e4"


        describe "the half move count-part" $ do
            let encoded = part 4. FEN.fen

            it "encodes move count 17 as '17'" $ do
                let p = pos & halfMoveCount .~ 17

                encoded p `shouldBe` "17"


        describe "the full move count-part" $ do
            let encoded = part 5. FEN.fen

            it "encodes move count 23 as '23'" $ do
                let p = pos & fullMoveCount .~ 23

                encoded p `shouldBe` "23"
