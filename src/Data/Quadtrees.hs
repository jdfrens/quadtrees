module Data.Quadtrees where

class QuadtreeMatrix qm where
  isZero :: qm -> Bool
  zero :: qm
  isIdent :: qm -> Bool
  isIdent _ = False
  isScalar :: qm -> Bool
  scalarAdd :: qm -> qm -> qm
  scalarSubtract :: qm -> qm -> qm
  scalarMult :: qm -> qm -> qm
  isQuad :: qm -> Bool
  mkQuad :: (qm, qm, qm, qm) -> qm
  nw :: qm -> qm
  ne :: qm -> qm
  sw :: qm -> qm
  se :: qm -> qm

data Matrix a = ZeroM | IdentM | ScalarM a |
  QuadM (Matrix a, Matrix a, Matrix a, Matrix a)
    deriving (Show, Eq)

instance (Eq a, Num a) => Num (Matrix a) where
  fromInteger 0 = ZeroM

  (+) = (#+#)
  (*) = (#*#)
  
  negate ZeroM = ZeroM
  negate (ScalarM s) = ScalarM $ negate s
  negate (QuadM q) = mkQuad $ mapQuad negate q
  negate m = error $ "cannot negate " ++ show m 

  abs _ = ZeroM
  signum _ = 0
  
instance (Num a) => QuadtreeMatrix (Matrix a) where
  isZero ZeroM = True
  isZero _ = False
  zero = ZeroM
  
  isIdent IdentM = True
  isIdent _ = False
  
  isScalar (ScalarM _) = True
  isScalar _ = False
  
  isQuad (QuadM _) = True
  isQuad _ = False
  
  mkQuad = normalize . QuadM
  
  scalarAdd (ScalarM a) (ScalarM b) = normalize $ ScalarM (a+b)
  scalarSubtract (ScalarM a) (ScalarM b) = normalize $ ScalarM (a-b)
  scalarMult (ScalarM a) (ScalarM b) = ScalarM (a*b)
  
  nw ZeroM = ZeroM
  nw (QuadM (q, _, _, _)) = q
  ne ZeroM = ZeroM
  ne (QuadM (_, q, _, _)) = q
  sw ZeroM = ZeroM
  sw (QuadM (_, _, q, _)) = q
  se ZeroM = ZeroM
  se (QuadM (_, _, _, q)) = q
    
infix 6 #+#
(#+#) :: (QuadtreeMatrix a) => a -> a -> a
a #+# b
  | isZero a = b
  | isZero b = a
  | isScalar a && isScalar b = scalarAdd a b
  | isQuad a && isQuad b = mkQuad $ zipQM (#+#) a b
  | otherwise = error "incompatible arguments"
  
infix 6 #-#
(#-#) :: (Num a, QuadtreeMatrix a) => a -> a -> a
a #-# b
  | isZero a = negate b
  | isZero b = a
  | isScalar a && isScalar b = scalarSubtract a b
  | isQuad a && isQuad b = mkQuad $ zipQM (#-#) a b
  | otherwise = error "incompatible arguments"
  
infix 7 #*#
(#*#) :: (QuadtreeMatrix a) => a -> a -> a
a #*# b
  | isZero a || isZero b = zero
  | isIdent a = b
  | isIdent b = a
  | isScalar a = scalarMult a b
  | isQuad a && isQuad b =
    let
      first  = mkQuad $ zipQM (#*#) (permute (ne, nw, se, sw) a) (permute (sw, ne, sw, ne) b)
      second = mkQuad $ zipQM (#*#) a (permute (nw, se, nw, se) b) in
    mkQuad $ zipQM (#+#) first second
  | otherwise = error "incompatible arguments"

permute (nw, ne, sw, se) quad = mkQuad (nw quad, ne quad, sw quad, se quad)

zipQM f a b =
  (f (nw a) (nw b), f (ne a) (ne b), f (sw a) (sw b), f (se a) (se b))
  
normalize (ScalarM 0) = ZeroM
normalize (QuadM (ZeroM, ZeroM, ZeroM, ZeroM)) = ZeroM
normalize (QuadM (IdentM, ZeroM, ZeroM, IdentM)) = IdentM
normalize q = q

mapQuad f (nw, ne, sw, se) = (f nw, f ne, f sw, f se)
zipQuads f (nw1, ne1, sw1, se1) (nw2, ne2, sw2, se2) =
  (f nw1 nw2, f ne1 ne2, f sw1 sw2, f se1 se2)
