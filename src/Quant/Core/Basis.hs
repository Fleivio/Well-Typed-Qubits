module Core.Basis(basis, Basis, Bit(..), toBool) where

import Core.Bit

class Ord a => Basis a where
  basis' :: [a]

instance Basis Bit where
  basis' = [1, 0]

basis :: forall a n. (Integral n, Basis a) => n -> [[a]]
basis 0 = [[]]
basis n = do 
  k <- basis' @a
  ks <- basis (n-1)
  return $ k : ks