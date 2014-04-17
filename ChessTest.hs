module HelpersTest where
import Control.Monad.Reader (runReader)
import Control.Applicative ((<*>), (<$>))
import Test.QuickCheck
import Data.Map (fromList)
import Data.Maybe (fromJust)
import Types as Chess
import qualified Fen
import Chess

instance Arbitrary Chess.Square where
  arbitrary = Chess.Square <$> choose ('a','h') <*> choose (1,8)

instance Arbitrary Chess.Piece where
  arbitrary = Chess.Piece <$> arbitrary <*> arbitrary

instance Arbitrary Chess.Color where
  arbitrary = elements [Black, White]

instance Arbitrary Chess.PieceType where
  arbitrary = elements $ [Pawn] ++ (Officer <$> enumFromTo Knight King)

instance Arbitrary Chess.OfficerType where
  arbitrary = oneof $ map return $ enumFromTo Knight King

arbitraryBoard :: Gen Chess.Board
arbitraryBoard =
  let kvs = zip <$> listOf1 arbitrary <*> listOf1 arbitrary
  in fromList <$> kvs
