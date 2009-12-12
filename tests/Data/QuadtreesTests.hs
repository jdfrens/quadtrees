module Data.QuadtreesTests where

import Test.HUnit
import Test.HUnit.Helpers

import Data.Quadtrees
  
tests = 
  testTree "quadtree tests" [
    testTree "as instance of quadtree test" [
      testCases "is zero?" [
          True @=? isZero ZeroM
        , False @=? isZero (ScalarM 4)
        , False @=? isZero (QuadM (ScalarM 1, ScalarM 1, ZeroM, ZeroM))
      ],
      testCases "is scalar?" [
          False @=? isScalar ZeroM
        , True @=? isScalar (ScalarM 4)
        , False @=? isScalar (QuadM (ScalarM 1, ScalarM 1, ZeroM, ZeroM))
      ]
    ],
    
    testCases "negate" [
        ZeroM @=? negate ZeroM
      , ScalarM (-4) @=? negate (ScalarM 4)
      , ScalarM 15 @=? negate (ScalarM (-15))
      , let q = QuadM (ScalarM 3, ScalarM 8, 
                       ScalarM 2, ScalarM 1) 
            q' = QuadM (ScalarM (-3), ScalarM (-8), 
                        ScalarM (-2), ScalarM (-1))
        in
        q' @=? negate q
    ],
    
    testTree "generic addition" [
      testCases "zero" [
          (ZeroM :: Matrix Int) @=? ZeroM #+# ZeroM
        , ScalarM 8 @=? ScalarM 8 #+# ZeroM
        , ScalarM 3 @=? ZeroM #+# ScalarM 3
        , let q = QuadM (ScalarM 3, ScalarM 8, ScalarM 2, ScalarM 1) in
          q @=? q #+# ZeroM
        , let q = QuadM (ScalarM 3, ScalarM 8, ScalarM 2, ScalarM 1) in
          q @=? ZeroM #+# q
        ],
      testCases "scalar" [
          ScalarM  8 @=? ScalarM  3 #+# ScalarM 5
        , ScalarM 17 @=? ScalarM 11 #+# ScalarM 6
        ],
      testCases "scalar normalization" [
          ZeroM @=? ScalarM 4 #+# ScalarM (-4)
        ],
      testCases "recursive" [
          QuadM (ScalarM 3, ScalarM 8, ScalarM 2, ScalarM 17)
          @=?
          QuadM (ScalarM 2, ScalarM 4, ScalarM 4, ScalarM 8)
          #+#
          QuadM (ScalarM 1, ScalarM 4, ScalarM (-2), ScalarM 9)  
        , QuadM (ScalarM 8, ScalarM 4, ScalarM 9, ScalarM 18)
          @=?
          QuadM (ScalarM 3, ZeroM,     ScalarM 4, ScalarM 9)
          #+#
          QuadM (ScalarM 5, ScalarM 4, ScalarM 5, ScalarM 9)
        ],
      testCases "recursive normalization" [
          ZeroM @=?
          QuadM (ScalarM (-5), ZeroM, ScalarM (-5), ScalarM 9)
          #+#
          QuadM (ScalarM 5, ZeroM, ScalarM 5, ScalarM (-9))
        ]
      ],
      
    testTree "generic subtraction" [
      testCases "zero" [
          (ZeroM :: Matrix Int) @=? ZeroM #-# ZeroM
        , ScalarM 8 @=? ScalarM 8 #-# ZeroM
        , ScalarM (-3) @=? ZeroM #-# ScalarM 3
        , let q = QuadM (ScalarM 3, ScalarM 8, ScalarM 2, ScalarM 1) in
          q @=? q #-# ZeroM
        , let q = QuadM (ScalarM 3, ScalarM 8, ScalarM 2, ScalarM 1) 
              q' = QuadM (ScalarM (-3), ScalarM (-8), ScalarM (-2), ScalarM (-1))
          in
          q' @=? ZeroM #-# q
        ],
      testCases "scalar" [
          ScalarM 3 @=? ScalarM  5 #-# ScalarM 2
        , ScalarM 5 @=? ScalarM 11 #-# ScalarM 6
        ],
      testCases "scalar normalization" [
          ZeroM @=? ScalarM 4 #-# ScalarM 4
        ],
      testCases "recursive" [
          QuadM (ScalarM 1, ScalarM 2, ScalarM 6, ScalarM (-1))
          @=?
          QuadM (ScalarM 2, ScalarM 6, ScalarM 4, ScalarM 8)
          #-#
          QuadM (ScalarM 1, ScalarM 4, ScalarM (-2), ScalarM 9)  
        , QuadM (ScalarM (-2), ScalarM (-4), ScalarM (-1), ScalarM 1)
          @=?
          QuadM (ScalarM 3, ZeroM,     ScalarM 4, ScalarM 10)
          #-#
          QuadM (ScalarM 5, ScalarM 4, ScalarM 5, ScalarM 9)
        ],
      testCases "recursive normalization" [
          ZeroM @=?
          QuadM (ScalarM 5, ZeroM, ScalarM (-5), ScalarM 9)
          #-#
          QuadM (ScalarM 5, ZeroM, ScalarM (-5), ScalarM 9)
        ]
      ],
      
    testTree "generic multiplication" [
      testCases "zero" [
          ZeroM @=? ZeroM #*# ZeroM
        , ZeroM @=? IdentM #*# ZeroM
        , ZeroM @=? ZeroM #*# IdentM
        , ZeroM @=? ScalarM 8 #*# ZeroM
        , ZeroM @=? ZeroM #*# ScalarM 3
        , let q = QuadM (ScalarM 3, ScalarM 8, ScalarM 2, ScalarM 1) in
          ZeroM @=? q #*# ZeroM
        , let q = QuadM (ScalarM 3, ScalarM 8, ScalarM 2, ScalarM 1) in
          ZeroM @=? ZeroM #*# q
        ],
      testCases "identity" [
          IdentM @=? IdentM #*# IdentM
        , ScalarM 8 @=? ScalarM 8 #*# IdentM
        , ScalarM 8 @=? IdentM #*# ScalarM 8
        , let q = QuadM (ScalarM 3, ScalarM 8, ScalarM 2, ScalarM 1) in
          q @=? q #*# IdentM
        , let q = QuadM (ScalarM 3, ScalarM 8, ScalarM 2, ScalarM 1) in
          q @=? IdentM #*# q
      ],
      testCases "scalar" [
          ScalarM 15 @=? ScalarM  3 #*# ScalarM 5
        , ScalarM 66 @=? ScalarM 11 #*# ScalarM 6
        ],
      testCases "recursive" [
          QuadM (ScalarM (-6), ScalarM 44, ScalarM (-12), ScalarM 88)
          @=?
          QuadM (ScalarM 2, ScalarM 4, ScalarM 4, ScalarM 8)
          #*#
          QuadM (ScalarM 1, ScalarM 4, ScalarM (-2), ScalarM 9)  
        , QuadM (ScalarM 15, ScalarM 12, ScalarM 65, ScalarM 97)
          @=?
          QuadM (ScalarM 3, ZeroM,     ScalarM 4, ScalarM 9)
          #*#
          QuadM (ScalarM 5, ScalarM 4, ScalarM 5, ScalarM 9)
        ],
      testCases "recursive normalization" [
          ZeroM @=?
          QuadM (ScalarM (-5), ZeroM, ScalarM (-5), ZeroM)
          #*#
          QuadM (ZeroM, ZeroM, ZeroM, ScalarM (-9))
        ]
      ],

    testCases "fromInteger" [
      ZeroM  @=? fromInteger 0
      ],
    
    testTree "addition" [
      testCases "zero" [
          ZeroM     @=? ZeroM + ZeroM
        , ScalarM 8 @=? ScalarM 8 + ZeroM
        , ScalarM 3 @=? ZeroM + ScalarM 3
        , let q = QuadM (ScalarM 3, ScalarM 8, ScalarM 2, ScalarM 1) in
          q @=? q + ZeroM
        , let q = QuadM (ScalarM 3, ScalarM 8, ScalarM 2, ScalarM 1) in
          q @=? ZeroM + q
        ],
      testCases "scalar" [
          ScalarM  8 @=? ScalarM  3 + ScalarM 5
        , ScalarM 17 @=? ScalarM 11 + ScalarM 6
        ],
      testCases "scalar normalization" [
          ZeroM @=? ScalarM 4 + ScalarM (-4)
        ],
      testCases "recursive" [
          QuadM (ScalarM 3, ScalarM 8, ScalarM 2, ScalarM 17)
          @=?
          QuadM (ScalarM 2, ScalarM 4, ScalarM 4, ScalarM 8)
          +
          QuadM (ScalarM 1, ScalarM 4, ScalarM (-2), ScalarM 9)  
        , QuadM (ScalarM 8, ScalarM 4, ScalarM 9, ScalarM 18)
          @=?
          QuadM (ScalarM 3, ZeroM,     ScalarM 4, ScalarM 9)
          +
          QuadM (ScalarM 5, ScalarM 4, ScalarM 5, ScalarM 9)
        ],
      testCases "recursive normalization" [
          ZeroM @=?
          QuadM (ScalarM (-5), ZeroM, ScalarM (-5), ScalarM 9)
          +
          QuadM (ScalarM 5, ZeroM, ScalarM 5, ScalarM (-9))
        ]
      ],
      
    testTree "normalization" [
      testCases "identity" [
          IdentM @=? normalize (QuadM (IdentM, ZeroM, ZeroM, IdentM))
        , let q = QuadM (IdentM, ScalarM 8, ScalarM 2, IdentM) in
          q @=? normalize q
        , let q = QuadM (IdentM, IdentM, IdentM, IdentM) in
          q @=? normalize q
        , let q = QuadM (ZeroM, IdentM, IdentM, ZeroM) in
          q @=? normalize q
      ]
    ],
      
    testTree "multiplication" [
      testCases "zero" [
          ZeroM @=? ZeroM * ZeroM
        , ZeroM @=? IdentM * ZeroM
        , ZeroM @=? ZeroM * IdentM
        , ZeroM @=? ScalarM 8 * ZeroM
        , ZeroM @=? ZeroM * ScalarM 3
        , let q = QuadM (ScalarM 3, ScalarM 8, ScalarM 2, ScalarM 1) in
          ZeroM @=? q * ZeroM
        , let q = QuadM (ScalarM 3, ScalarM 8, ScalarM 2, ScalarM 1) in
          ZeroM @=? ZeroM * q
        ],
      testCases "identity" [
          IdentM @=? IdentM * IdentM
        , ScalarM 8 @=? ScalarM 8 * IdentM
        , ScalarM 8 @=? IdentM * ScalarM 8
        , let q = QuadM (ScalarM 3, ScalarM 8, ScalarM 2, ScalarM 1) in
          q @=? q * IdentM
        , let q = QuadM (ScalarM 3, ScalarM 8, ScalarM 2, ScalarM 1) in
          q @=? IdentM * q
      ],
      testCases "scalar" [
          ScalarM 15 @=? ScalarM  3 * ScalarM 5
        , ScalarM 66 @=? ScalarM 11 * ScalarM 6
        ],
      testCases "recursive" [
          QuadM (ScalarM (-6), ScalarM 44, ScalarM (-12), ScalarM 88)
          @=?
          QuadM (ScalarM 2, ScalarM 4, ScalarM 4, ScalarM 8)
          *
          QuadM (ScalarM 1, ScalarM 4, ScalarM (-2), ScalarM 9)  
        , QuadM (ScalarM 15, ScalarM 12, ScalarM 65, ScalarM 97)
          @=?
          QuadM (ScalarM 3, ZeroM,     ScalarM 4, ScalarM 9)
          *
          QuadM (ScalarM 5, ScalarM 4, ScalarM 5, ScalarM 9)
        ],
      testCases "recursive normalization" [
          ZeroM @=?
          QuadM (ScalarM (-5), ZeroM, ScalarM (-5), ZeroM)
          *
          QuadM (ZeroM, ZeroM, ZeroM, ScalarM (-9))
        ]
      ]
    ]
  
-- 2 4    1 4
-- 4 8   -2 9

-- 3 0    5 4
-- 4 9    5 9