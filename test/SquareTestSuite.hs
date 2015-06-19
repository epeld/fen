module SquareTestSuite where
import TestSquare
import Distribution.TestSuite


tests :: IO [Test]
tests = return [ Test succeeds ]
  where
    succeeds = TestInstance
        { run = do { TestSquare.main; return $ Finished Pass }
        , name = "Square"
        , tags = []
        , options = []
        , setOption = \_ _ -> Right succeeds
        }
