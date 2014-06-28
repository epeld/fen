module TestUtils where
import Test.HUnit.Base
import Test.HUnit.Text

assertElems :: (Show a, Eq a) => String -> [a] -> [a] -> Assertion
assertElems s els x = sequence_ $ map (\el -> assertElem s el x) els

assertElem :: (Show a, Eq a) => String -> a -> [a] -> Assertion
assertElem s a as =
  let s' = unwords [s, ". Expected:", show a, "in", show as]
  in assertBool s' (elem a as)
