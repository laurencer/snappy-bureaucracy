import Test.Tasty
import Test.Tasty.HUnit

import Data.Maybe

import Text.Trifecta.Parser

import System.IO.Unsafe

import Bureaucracy.Thrift.LanguageThrift

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ unitTests ]

unitTests = testGroup "Unit tests"
  [ testCase "Can parse `ChangeRecords`" $ isJust (unsafePerformIO $ do
        result <- parseFromFile document "thrift/ChangeRecords.thrift"
        putStrLn $ show result
        return result
      ) @?= True
  ]