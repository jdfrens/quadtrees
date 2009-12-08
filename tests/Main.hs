import Test.HUnit

import Data.QuadtreesTests
import Data.BintreesTests

allTests = TestList $ Data.BintreesTests.tests : map TestCase Data.QuadtreesTests.assertions

main = runTestTT allTests
