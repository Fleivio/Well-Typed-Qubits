module List.SList(SList(..), sListToList, SNat(..)) where

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