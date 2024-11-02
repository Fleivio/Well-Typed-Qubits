module List.NList(NList(..)) where

import Data.Kind
import GHC.TypeLits
import Unsafe.Coerce

type NList :: Type -> Natural -> Type
data NList a n where
  NNil :: NList a 0
  (:>) :: a -> NList a n -> NList a (n + 1)
infixr 5 :>

nListToList :: NList a n -> [a]
nListToList = unsafeCoerce

instance (Show a) => Show (NList a n) where
  show a = "#" ++ show (nListToList a)