module Data.BintreesTests where

import Test.HUnit
import Data.Bintrees
  
tests = 
  TestLabel "binvector tests" $
    TestList [
      TestLabel "fromInteger" $ TestList $ map TestCase [
        ZeroV @=? fromInteger 0
      ],
  
      TestLabel "addition" $
        TestList [
          TestLabel "zero" $ TestList $ map TestCase [
              ZeroV @=? ZeroV + ZeroV
            , ScalarV 8 @=? ScalarV 8 + ZeroV
            , ScalarV 5 @=? ZeroV + ScalarV 5
          ],
          TestLabel "scalars" $ TestList $ map TestCase [
              ScalarV  3 @=? ScalarV 2 + ScalarV 1
            , ScalarV 12 @=? ScalarV 4 + ScalarV 8
          ],
          TestLabel "normalized scalars" $ TestList $ map TestCase [
              ZeroV @=? ScalarV 2 + ScalarV (-2)
          ],
          TestLabel "recursive" $ TestList $ map TestCase [
              BinV (ScalarV 8, ScalarV 5)
              @=? 
              BinV (ScalarV 2, ScalarV 1)
              +
              BinV (ScalarV 6, ScalarV 4)
            , BinV (ScalarV 10, ScalarV 15)
              @=? 
              BinV (ScalarV (-2), ScalarV 5)
              +
              BinV (ScalarV 12, ScalarV 10)
          ],
          TestLabel "normalized recursive" $ TestList $ map TestCase [
              ZeroV
              @=? 
              BinV (ScalarV 5, ScalarV (-3)) + BinV (ScalarV (-5), ScalarV 3)
          ]
        ]
    ]
