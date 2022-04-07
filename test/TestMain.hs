module Main where

import System.Exit
import Test.HUnit
import TestAlloc
import TestParser

allTests :: Test
allTests = test ["parser" ~: parserTests, "allocator" ~: testAlloc]

main :: IO ()
main = do
  count <- runTestTT allTests
  if errors count + failures count == 0
    then exitSuccess
    else exitFailure
