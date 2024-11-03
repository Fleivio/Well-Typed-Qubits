module List.SList(
  SList(..)
  , sListToList
  , Length
  , module GHC.TypeLits
  , ValidSelector) where

import Data.Kind
import GHC.TypeLits
import Unsafe.Coerce
import Fcf hiding (Length, type(+))

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


type Elem :: Natural -> [Natural] -> Bool
type family Elem a as
 where
  Elem a '[]      = 'False
  Elem a (a ': as) = 'True
  Elem a (b ': as) = Elem a as

type HasRepetition :: [Natural] -> Bool
type family HasRepetition xs
 where
  HasRepetition '[] = 'False
  HasRepetition (x ': xs) = If (Elem x xs) 'True (HasRepetition xs)

type Maximum :: [Natural] -> Natural
type family Maximum a
 where
  Maximum '[]       = TypeError (Text "Unable to Eval Maximum of a empty list")
  Maximum (x : '[]) = x
  Maximum (x : xs)  = If (x <=? Maximum xs) (Maximum xs) x

type HasZero :: [Natural] -> Bool
type family HasZero n 
  where 
    HasZero '[] = 'False
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
  = If (HasRepetition xs)
    (TypeError (
        Text "No Cloning Theorem Violation" 
        :$$: 
        Text "You tried to select qubits with repetition " :<>: ShowType xs
        ))
    (() :: Constraint) 

type NoZeroCheck :: [Natural] -> Constraint
type NoZeroCheck xs
  = If (HasZero xs)
    (TypeError (
        Text "Zero qubit selection is not allowed" 
        :$$:
        Text "The qubit selection list starts from 1"
        ))
    (() :: Constraint)

type ValidSelector :: [Natural] -> Natural -> Constraint
type ValidSelector xs n 
  = (BoundCheck n xs, NoCloningCheck xs, NoZeroCheck xs)