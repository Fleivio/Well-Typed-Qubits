module List.SList(
  SList(..)
  , sListToList
  , Length
  , module GHC.TypeLits
  , type (<++>)
  , ValidSelector
  , sListRange
  , sListConcat) where

import Data.Kind
import GHC.TypeLits
import Data.Type.Bool
import Unsafe.Coerce
import Data.Proxy (Proxy(..))

type SList :: [Natural] -> Type
data SList as where
  SNil :: SList '[]
  (:-) :: SNat a -> SList as -> SList (a ': as)
infixr 5 :-

sListToList :: SList as -> [Int]
sListToList = unsafeCoerce

type family (as :: [k]) <++> (bs :: [k]) :: [k] where
  '[] <++> bs = bs
  (a ': as) <++> bs = a ': (as <++> bs)

sListConcat :: SList a -> SList b -> SList (a <++> b)
sListConcat = unsafeCoerce (++)

sListRange :: forall lb ub. (KnownNat lb, KnownNat ub) => SList (NatRange lb ub)
sListRange = let list = fromIntegral <$> [natVal (Proxy @lb)..natVal (Proxy @ub)] :: [Int]
  in unsafeCoerce list

type NatRange :: Natural -> Natural -> [Natural]
type family NatRange lb up where
  NatRange ub ub = '[ub]
  NatRange lb ub = If (ub <=? lb) 
                    (TypeError (Text "SList lower bound is greater than the upper bound"))
                    (lb ': NatRange (lb + 1) ub)

type family Length (as :: [k]) :: Natural where
  Length '[] = 0
  Length (a ': as) = 1 + Length as

type Elem :: Natural -> [Natural] -> Bool
type family Elem a as
 where
  Elem a '[]      = 'False
  Elem a (a ': as) = 'True
  Elem a (b ': as) = Elem a as

type family BoundCheck (n :: Natural) (ns :: [Natural]) :: Constraint where
  BoundCheck n '[]    = ()
  BoundCheck n (k:ks) = If (k <=? n) (BoundCheck n ks)
        (TypeError (
        Text "Index out of bounds on Qubit selection" 
        :$$: 
        Text "You got " :<>: ShowType n :<>: Text " qubits" :$$: Text "But tried to select qubit no" :<>: ShowType k
        ))

type family NoCloningCheck (xs :: [Natural]) :: Constraint
  where
  NoCloningCheck '[] = ()
  NoCloningCheck (x ': xs) = If (x `Elem` xs)
    (TypeError (
        Text "No Cloning Theorem Violation" 
        :$$: 
        Text "You've tried to select qubits with repetition: " :<>: ShowType x
        ))
    (NoCloningCheck xs) 

type family NoZeroCheck (xs :: [Natural]) :: Constraint
  where
    NoZeroCheck '[] = ()
    NoZeroCheck (0 : xs) = (TypeError (
                            Text "Zero qubit selection is not allowed" 
                            :$$:
                            Text "The qubit selection list starts from 1"
                            ))
    NoZeroCheck (x : xs) = NoZeroCheck xs

type ValidSelector :: [Natural] -> Natural -> Constraint
type ValidSelector xs n
  = (NoCloningCheck xs, BoundCheck n xs, NoZeroCheck xs)
