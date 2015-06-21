module TestUtils where
import Test.HUnit
import Test.Hspec

import Control.Monad.Reader
import Data.Maybe

import PositionReader
import FENDecode
import Position

test :: IO ()
test = hspec $
    describe "Helpers" $ do
        -- TODO we really want to test FAILING cases here but don't know how..
        
        describe "assertRight" $ do
            it "assertRight Right" $ assertRight (Right 3 :: Either Int Int)

        describe "withInitialPosition" $ do
            withInitialPosition $ do
                --return $ it "blows up" $ 3 `shouldBe` 4
                return $ it "works " $ 3 `shouldBe` 3


withInitialPosition :: (Position -> Spec) -> Spec
withInitialPosition = withPosition initialFEN

withPosition :: String -> (Position -> Spec) -> Spec
withPosition fen f =
    let label = "position " ++ fen
        spc = decode fen `withRight` f
    in label `describe` spc


withRight :: (Show a, Show b) => Either a b -> (b -> Spec) -> Spec
withRight x f = do
    it "is valid" (assertRight x)
    withRight' x f

withRight' :: Monad m => Either a b -> (b -> m ()) -> m ()
withRight' (Right a) f = f a
withRight' _ _ = return ()

withJust' :: Monad m => Maybe a -> (a -> m ()) -> m () 
withJust' (Just x) f = f x
withJust' _ _ = return ()

assertRight x = x `shouldSatisfy` isRight
assertJust x = x `shouldSatisfy` isJust

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

