module List.Vec(Vec(..), unsafeVec, vecToList) where


import Data.Kind
import GHC.TypeLits

import Unsafe.Coerce

type Vec :: Natural -> Type -> Type
data Vec n a where
  VNil :: Vec 0 a
  (:>) :: a -> Vec n a -> Vec (n + 1) a
infixr 5 :>

instance Functor (Vec n) where
  fmap f ls = unsafeCoerce $ f <$> vecToList ls

vecToList :: Vec n a -> [a]
vecToList = unsafeCoerce

unsafeVec :: forall n a. KnownNat n => [a] -> Vec n a
unsafeVec = unsafeCoerce

instance (Show a) => Show (Vec n a) where
  show a = "#" ++ show (vecToList a)

instance (Eq a) => Eq (Vec n a) where
  a == b = vecToList a == vecToList b

instance Foldable (Vec n) where
  foldr f b ta = foldr f b (vecToList ta)