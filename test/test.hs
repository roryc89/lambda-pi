import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.LambdaPi.Infer

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ Test.LambdaPi.Infer.tests
  ] 