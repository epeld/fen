{-# LANGUAGE TemplateHaskell #-}
module MoveInference where
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty)
import Data.Traversable

import Control.Lens
import Control.Monad.Except

import Position
import PartialMove
import LegalMove
import qualified MoveVerification as Verification
import qualified PositionUpdates as Updated


data Error = Error { _move :: PartialMove, _position :: Position, _reasons :: [Verification.Error] }
    deriving (Show, Eq)


makeLenses ''Error


afterLast :: Position -> NonEmpty PartialMove -> Either Error Position
afterLast p mvs = NonEmpty.last (afterAll p mvs)


afterAll :: Position -> NonEmpty PartialMove -> NonEmpty (Either Error Position)
afterAll pos mvs = NonEmpty.fromList ([Right pos] ++ rights ++ take 1 lefts)
    where
    (rights, lefts) = NonEmpty.span isRight positions

    (_, positions) = mapAccumL accumulator (Right pos) mvs

    accumulator p mv = (make p mv, make p mv)

    make (Right p) mv = after p mv
    make (Left err) _ = Left err

    isRight (Right _) = True
    isRight _ = False


after :: Position -> PartialMove -> Either Error Position
after pos mv = case runExcept (fullMove pos mv) of
    Left err -> Left (Error mv pos err)
    Right fmv -> Right (Updated.after pos fmv)
