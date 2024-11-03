module List.SList(SList(..), sListToList, SNat(..), Length, module GHC.TypeLits) where

import Data.Kind
import GHC.TypeLits
import Unsafe.Coerce
import Data.Proxy

type SList :: [Natural] -> Type
data SList as where
  SNil :: SList '[]
  (:-) :: SNat a -> SList as -> SList (a ': as)
infixr 5 :-

sListToList :: SList as -> [Int]
sListToList = unsafeCoerce

type family Length (as :: [k]) :: Nat where
  Length '[] = 0
  Length (a ': as) = 1 + Length as