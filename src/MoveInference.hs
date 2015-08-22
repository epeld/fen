module MoveInference where
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty)
import Data.Traversable

import Control.Lens
import Control.Monad.Except

import Position
import PartialMove
import MoveVerification as Verification
import LegalMove
import PositionUpdates as Updated


data Error = Error { _move :: PartialMove, _position :: Position, _reasons :: [Verification.Error] }
    deriving (Show, Eq)


afterLast :: Position -> NonEmpty PartialMove -> Either [Error] Position
afterLast p mvs = NonEmpty.last (afterAll p mvs)


afterAll :: Position -> NonEmpty PartialMove -> [Either [Error] Position]
afterAll pos mvs = rights ++ take 1 lefts)
    where
    (rights, lefts) = NonEmpty.span isRight positions

    (_, positions) = mapAccumL accumulator (Right pos) mvs

    accumulator p mv = (make p mv, make p mv)

    make (Right p) mv = afterPartial p mv
    make (Left err) _ = Left err

    isRight (Right _) = True
    isRight _ = False


makeLenses ''Error


after :: Position -> PartialMove -> Except [Error] Position
after pos mv = case runExcept (fullMove pos mv) of
    Left err -> Error mv pos err
    Right fmv -> Updated.after pos fmv
