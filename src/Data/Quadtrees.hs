module Data.Quadtrees where
  
type Quadrants a = (Matrix a, Matrix a, Matrix a, Matrix a)
data Matrix a = ZeroM | ScalarM a | QuadM (Quadrants a)
  deriving (Show, Eq)

instance (Eq a, Num a) => Num (Matrix a) where
  fromInteger 0 = ZeroM

  ZeroM + m = m
  m + ZeroM = m
  ScalarM a + ScalarM b = normalize $ ScalarM (a+b)
  QuadM a + QuadM b = normalize $ QuadM $ zipQuads (+) a b
  _ + _ = error "invalid arguments"

  ZeroM * _ = ZeroM
  _ * ZeroM = ZeroM
  ScalarM a * ScalarM b = ScalarM (a*b)
  QuadM a@(nwA, neA, swA, seA) * QuadM b@(nwB, neB, swB, seB) =
    normalize $ QuadM $ zipQuads (+) (zipQuads (*) x y) (zipQuads (*) a z)
      where x = (neA, nwA, seA, swA)
            y = (swB, neB, swB, neB)
            z = (nwB, seB, nwB, seB)
  _ * _ = error "invalid arguments"
  
  
  abs _ = ZeroM
  signum _ = 0
  
normalize (ScalarM 0) = ZeroM
normalize (QuadM (ZeroM, ZeroM, ZeroM, ZeroM)) = ZeroM
normalize q = q

zipQuads f (nw1, ne1, sw1, se1) (nw2, ne2, sw2, se2) =
  (f nw1 nw2, f ne1 ne2, f sw1 sw2, f se1 se2)
  