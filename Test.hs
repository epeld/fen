module Test where
import Test.HUnit
import Test.Hspec

import Control.Monad.Reader

import PositionReader
import FENDecode

withPosition :: String -> PReader Spec -> Spec
withPosition fen r =
    let p = decode fen
        text = "position " ++ fen
    in describe text $ do
        it "is valid" $ assertRight p
        withRight p $ runReader r

assertRight (Left err) = assertFailure $ show err
assertRight _ = assertSuccess
assertSuccess = return ()

withRight :: Monad m => Either a b -> (b -> m ()) -> m ()
withRight (Right a) f = f a
withRight _ _ = return ()

--withInitialPosition :: String -> Spec
--withInitialPosition = withPosition initialFEN

{-
stuff = withInitialPosition $ do
    hasOfficerAt Knight "e4"
    hasPawnAt "d2"
    hasPawnAt "e2"
    afterMoves "e4 e5 Nc3" $ do
        hasPawnAt "e4"
        hasNoPieceAt "e2"

withSquare :: String -> Reader Square Spec -> Spec
withSquare s =
    describe ("square " ++ s) $
        case s of
            Nothing -> -}

afterMoves :: String -> PReader Spec -> Spec
afterMoves m r = 
    describe ("moves " ++ m) $ do
        let mvs = parseMoves m
        it "are parsed correctly" $ assertRight mvs
        withRight mvs $ \mvs' -> do
            let p = afterMany mvs'
            it "produces a legal position" $ assertRight p
            withRight p $ \p' ->
                describe "in resulting position" $
                    runReader r p'
        
