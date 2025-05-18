module List.Vec(Vec(..)) where

import Data.Kind
import GHC.TypeLits
import Unsafe.Coerce

type Vec :: Natural -> Type -> Type
data Vec n a where
  VNil :: Vec 0 a
  (:>) :: a -> Vec n a -> Vec (n + 1) a
infixr 5 :>

instance Functor (Vec n) where
  fmap f ls = unsafeCoerce $ f <$> nListToList ls

nListToList :: Vec n a -> [a]
nListToList = unsafeCoerce

instance (Show a) => Show (Vec n a) where
  show a = "#" ++ show (nListToList a)

instance (Eq a) => Eq (Vec n a) where
  a == b = nListToList a == nListToList b

instance Foldable (Vec n) where
  foldr f b ta = foldr f b (nListToList ta)