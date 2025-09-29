{-# OPTIONS_GHC -Wno-orphans #-}
module Quoters.NatLabel() where

import GHC.OverloadedLabels (IsLabel(..))
import GHC.TypeLits
import qualified GHC.TypeLits as GT

type family SRead (s :: Symbol) :: Natural where
  SRead inputSymbol = MultiplicationStep (UnconsSymbol inputSymbol)

type family MultiplicationStep (s :: Maybe (Char, Symbol)) :: Natural where
  MultiplicationStep 'Nothing = 0
  MultiplicationStep ('Just '(c, rest)) = ParseChar c GT.* (10 GT.^ SymbLength rest) + SRead rest

type family SymbLength (s :: Symbol) :: Natural where
  SymbLength "" = 0
  SymbLength sm = 1 + SymbLength (SndM (UnconsSymbol sm))

type family SndM (s :: Maybe (Char, Symbol)) :: Symbol where
  SndM 'Nothing = ""
  SndM ('Just '(c, rest)) = rest

type family ParseChar (c :: Char) :: Natural where
  ParseChar '0' = 0
  ParseChar '1' = 1
  ParseChar '2' = 2
  ParseChar '3' = 3
  ParseChar '4' = 4
  ParseChar '5' = 5
  ParseChar '6' = 6
  ParseChar '7' = 7
  ParseChar '8' = 8
  ParseChar '9' = 9

instance (SRead n ~ n1, KnownNat n1) => IsLabel n (SNat n1) where
  fromLabel = SNat @n1
