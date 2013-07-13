module TestUtils where
import Test.HUnit

assertLength l a = assertEqual s l $ length a
    where s = "length " ++ show l
