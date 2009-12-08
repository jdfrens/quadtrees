module Data.QuadtreesTests where

import Test.HUnit
import Data.Quadtrees
  
assertions = [
    ZeroM  @=? fromInteger 0
    
  -- ADDITION
  -- zero addition
  , ZeroM     @=? ZeroM + ZeroM
  , ScalarM 8 @=? ScalarM 8 + ZeroM
  , ScalarM 3 @=? ZeroM + ScalarM 3
  , let q = QuadM (ScalarM 3, ScalarM 8, ScalarM 2, ScalarM 1) in
    q @=? q + ZeroM
  , let q = QuadM (ScalarM 3, ScalarM 8, ScalarM 2, ScalarM 1) in
    q @=? ZeroM + q
    
  -- scalar addition
  , ScalarM  8 @=? ScalarM  3 + ScalarM 5
  , ScalarM 17 @=? ScalarM 11 + ScalarM 6
  -- normalization
  , ZeroM @=? ScalarM 4 + ScalarM (-4)
  
  -- recursive addition
  , QuadM (ScalarM 3, ScalarM 8, ScalarM 2, ScalarM 17)
    @=?
    QuadM (ScalarM 2, ScalarM 4, ScalarM 4, ScalarM 8)
    +
    QuadM (ScalarM 1, ScalarM 4, ScalarM (-2), ScalarM 9)  
  , QuadM (ScalarM 8, ScalarM 4, ScalarM 9, ScalarM 18)
    @=?
    QuadM (ScalarM 3, ZeroM,     ScalarM 4, ScalarM 9)
    +
    QuadM (ScalarM 5, ScalarM 4, ScalarM 5, ScalarM 9)  
  -- normalization
  , ZeroM @=?
    QuadM (ScalarM (-5), ZeroM, ScalarM (-5), ScalarM 9)
    +
    QuadM (ScalarM 5, ZeroM, ScalarM 5, ScalarM (-9))
    
    
  -- MULTIPLICATION
  -- zero multiplication
  , ZeroM @=? ZeroM * ZeroM
  , ZeroM @=? ScalarM 8 * ZeroM
  , ZeroM @=? ZeroM * ScalarM 3
  , let q = QuadM (ScalarM 3, ScalarM 8, ScalarM 2, ScalarM 1) in
    ZeroM @=? q * ZeroM
  , let q = QuadM (ScalarM 3, ScalarM 8, ScalarM 2, ScalarM 1) in
    ZeroM @=? ZeroM * q
    
  -- scalar multiplication
  , ScalarM 15 @=? ScalarM  3 * ScalarM 5
  , ScalarM 66 @=? ScalarM 11 * ScalarM 6

  -- recursive multiplication
  , QuadM (ScalarM (-6), ScalarM 44, ScalarM (-12), ScalarM 88)
    @=?
    QuadM (ScalarM 2, ScalarM 4, ScalarM 4, ScalarM 8)
    *
    QuadM (ScalarM 1, ScalarM 4, ScalarM (-2), ScalarM 9)  
  , QuadM (ScalarM 15, ScalarM 12, ScalarM 65, ScalarM 97)
    @=?
    QuadM (ScalarM 3, ZeroM,     ScalarM 4, ScalarM 9)
    *
    QuadM (ScalarM 5, ScalarM 4, ScalarM 5, ScalarM 9)  
  -- normalization
  , ZeroM @=?
    QuadM (ScalarM (-5), ZeroM, ScalarM (-5), ZeroM)
    *
    QuadM (ZeroM, ZeroM, ZeroM, ScalarM (-9))
  ]
  
-- 2 4    1 4
-- 4 8   -2 9

-- 3 0    5 4
-- 4 9    5 9