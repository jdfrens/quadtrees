import Test.HUnit

import Data.QuadtreesTests
import Data.Quadtrees.LUTests
import Data.BintreesTests

allTests = TestList [
  Data.BintreesTests.tests,
  Data.QuadtreesTests.tests,
  Data.Quadtrees.LUTests.tests
  ]
  
main = runTestTT allTests
