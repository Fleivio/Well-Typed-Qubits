module List.NList(NList(..)) where

import Data.Kind
import GHC.TypeLits
import Unsafe.Coerce

type NList :: Natural -> Type -> Type
data NList n a where
  NNil :: NList 0 a
  (:>) :: a -> NList n a -> NList (n + 1) a
infixr 5 :>

instance Functor (NList n) where
  fmap f ls = unsafeCoerce $ f <$> nListToList ls

nListToList :: NList n a -> [a]
nListToList = unsafeCoerce

instance (Show a) => Show (NList n a) where
  show a = "#" ++ show (nListToList a)

instance (Eq a) => Eq (NList n a) where
  a == b = nListToList a == nListToList b

instance Foldable (NList n) where
  foldr f b ta = foldr f b (nListToList ta)