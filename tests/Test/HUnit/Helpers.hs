module Test.HUnit.Helpers where

import Test.HUnit

testTree label tests = TestLabel label $ TestList tests
testCases label assertions =
  TestLabel label $ TestList $ map TestCase assertions
