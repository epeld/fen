module TestUtils where
import Test.HUnit
import Test.Hspec

import Control.Monad.Reader

import PositionReader
import FENDecode

withInitialPosition :: PReader Spec -> Spec
withInitialPosition = withPosition initialFEN

withPosition :: String -> PReader Spec -> Spec
withPosition fen r =
    let label = "position " ++ fen
        assertion = decode fen `withRight` runReader r
    in label `describe` assertion


withRight :: (Show a, Show b) => Either a b -> (b -> Spec) -> Spec
withRight x f = do
    it "is valid" (assertRight x)
    withRight' x f

withRight' :: Either a b -> (b -> Spec) -> Spec
withRight' (Right a) f = f a
withRight' _ _ = doNothing

assertRight x = x `shouldSatisfy` isRight

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

doNothing = return ()
