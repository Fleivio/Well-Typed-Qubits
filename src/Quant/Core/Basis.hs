module Core.Basis(basis, Basis, Bit(..)) where

import Core.Bit

class Ord a => Basis a where
  basis' :: [a]

instance Basis Bit where
  basis' = [O, I]

basis :: forall a. Basis a => Int -> [[a]]
basis 0 = [[]]
basis n = do 
  k <- basis' @a
  ks <- basis (n-1)
  return $ k : ks