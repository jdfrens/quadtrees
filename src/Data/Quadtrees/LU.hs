module Data.Quadtrees.LU where
  
import Data.Quadtrees
import Data.Bintrees

type Decomp a = (a, Matrix a, Vector Int, Matrix a, Matrix a)
solveLinear  :: (Num a) => Decomp a -> Vector a -> Vector a
solveLinear (_, plusLU', omega, p, q) b = (q #*| z) where
  y = forwardSubst plusLU' (p #*| b) omega
  z = backSubst plusLU' y omega

  forwardSubst :: (Num a) => Matrix a -> Vector a -> Vector Int -> Vector a
  --forwardSubst plusLU' c omega = y such that (I-L)y=c
  forwardSubst _ c (ScalarV _) = c -- I*c
  forwardSubst _ ZeroV _ = ZeroV
  forwardSubst (QuadM (plusLU'nw,_,w,plusLU'se)) (BinV (c_n, c_s)) (BinV (omega_n, omega_s)) = BinV (y_n, y_s) where
      y_n = forwardSubst plusLU'nw c_n omega_n
      y_s = forwardSubst plusLU'se (c_s + w #*| y_n) omega_s
  forwardSubst _ _ _ = error "Overconstrained"

  backSubst :: (Num a) => Matrix a -> Vector a -> Vector Int -> Vector a
  --backSubst plusLU' y omega = z such that Uz=y
  backSubst plusLU' y (ScalarV _) = plusLU' #*| y -- U'*y
  backSubst _ ZeroV _ = ZeroV
  backSubst (QuadM (plusLU'nw,e,_,plusLU'se)) (BinV (y_n, y_s)) (BinV (omega_n, omega_s)) = BinV (z_n, z_s) where
      z_n = backSubst plusLU'nw (y_n - e #*| z_s) omega_s
      z_s = backSubst plusLU'se y_s omega_n

  ZeroM #*| _ = ZeroV
  -- IdentM #*| v = v
  _ #*| ZeroV = ZeroV
  (ScalarM x) #*| (ScalarV y) = ScalarV (x * y)
  QuadM (nw,ne,sw,se) #*| (BinV (n, s)) = 
    case (zipBin (+)
      (nw #*| n, ne #*| s)
      (sw #*| n, se #*| s)) of
        (ZeroV,ZeroV) -> ZeroV
        (n' ,s' )     -> BinV (n', s')
