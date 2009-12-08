module Data.Bintrees where
  
data (Num a) => Vector a =	ZeroV | ScalarV a | BinV (Vector a, Vector a)
  deriving (Eq, Show)

instance (Num a) => Num (Vector a) where
  fromInteger 0 = ZeroV
  
  ZeroV + v = v
  v + ZeroV = v
  ScalarV x + ScalarV y = normalize $ ScalarV (x+y)
  BinV x + BinV y = normalize $ BinV (zipBin (+) x y)
  
  _ * _ = error "nothing"
  abs _ = error "nothing"
  signum _ = error "nothing"

normalize (ScalarV 0) = ZeroV
normalize (BinV (ZeroV, ZeroV)) = ZeroV
normalize v = v

zipBin f (xn, xs) (yn, ys) = (f xn yn, f xs ys)
