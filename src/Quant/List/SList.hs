module List.SList(
  SList(..)
  , sListToList
  , Length
  , module GHC.TypeLits
  , type (<++>)
  , ValidSelector
  , sListConcat) where

import Data.Kind
import GHC.TypeLits
import Unsafe.Coerce
import Fcf (If)



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

type family Length (as :: [k]) :: Natural where
  Length '[] = 0
  Length (a ': as) = 1 + Length as

type Elem :: Natural -> [Natural] -> Bool
type family Elem a as
 where
  Elem a '[]      = 'False
  Elem a (a ': as) = 'True
  Elem a (b ': as) = Elem a as

type HasDupl :: [Natural] -> Bool
type family HasDupl xs
 where
  HasDupl '[]       = 'False
  HasDupl (x ': xs) = If (Elem x xs) 'True (HasDupl xs)

type Maximum :: [Natural] -> Natural
type family Maximum a
 where
  Maximum '[]       = TypeError (Text "Unable to Eval Maximum of a empty list")
  Maximum (x : '[]) = x
  Maximum (x : xs)  = If (x <=? Maximum xs) (Maximum xs) x

type HasZero :: [Natural] -> Bool
type family HasZero n 
  where 
    HasZero '[]       = 'False
    HasZero (0 ': xs) = 'True
    HasZero (x ': xs) = HasZero xs

type BoundCheck :: Natural -> [Natural] -> Constraint
type BoundCheck n xs 
  = If (Maximum xs <=? n) (() :: Constraint) 
    (TypeError (
        Text "Index out of bounds on Qubit selection" 
        :$$: 
        Text "You got " :<>: ShowType n :<>: Text " qubits" :$$: Text "But tried to select qubits " :<>: ShowType xs
        ))

type NoCloningCheck :: [Natural] -> Constraint
type NoCloningCheck xs
  = If (HasDupl xs)
    (TypeError (
        Text "No Cloning Theorem Violation" 
        :$$: 
        Text "You tried to select qubits with repetition " :<>: ShowType xs
        ))
    (() :: Constraint) 

type NoZeroCheck :: [Natural] -> Constraint
type family NoZeroCheck xs where
  NoZeroCheck xs = If (HasZero xs)
    (TypeError (
        Text "Zero qubit selection is not allowed" 
        :$$:
        Text "The qubit selection list starts from 1"
        ))
    (() :: Constraint)

type ValidSelector :: [Natural] -> Natural -> Constraint
type ValidSelector xs n 
  = ( NoCloningCheck xs,BoundCheck n xs, NoZeroCheck xs)
