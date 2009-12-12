module Data.BintreesTests where

import Test.HUnit
import Test.HUnit.Helpers

import Data.Bintrees

tests = 
  testTree "binvector tests" [
      testCases "fromInteger" [
        ZeroV @=? fromInteger 0
      ],
  
      testTree "addition" [
          testCases "zero" [
              ZeroV @=? ZeroV + ZeroV
            , ScalarV 8 @=? ScalarV 8 + ZeroV
            , ScalarV 5 @=? ZeroV + ScalarV 5
          ],
          testCases "scalars" [
              ScalarV  3 @=? ScalarV 2 + ScalarV 1
            , ScalarV 12 @=? ScalarV 4 + ScalarV 8
          ],
          testCases "normalized scalars" [
              ZeroV @=? ScalarV 2 + ScalarV (-2)
          ],
          testCases "recursive" [
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
          testCases "normalized recursive" [
              ZeroV
              @=? 
              BinV (ScalarV 5, ScalarV (-3)) + BinV (ScalarV (-5), ScalarV 3)
          ]
        ]
    ]
