{-# OPTIONS_GHC -Wno-orphans #-}
module List.OvLabel() where

import GHC.OverloadedLabels (IsLabel(..))
import GHC.TypeLits

type family SRead (s :: Symbol) :: Natural where
  -- there is problably a better way to do this
  SRead "0" = 0
  SRead "1" = 1
  SRead "2" = 2
  SRead "3" = 3
  SRead "4" = 4
  SRead "5" = 5
  SRead "6" = 6
  SRead "7" = 7
  SRead "8" = 8
  SRead "9" = 9
  SRead "10" = 10
  SRead "11" = 11
  SRead "12" = 12
  SRead "13" = 13
  SRead "14" = 14
  SRead "15" = 15
  SRead "16" = 16
  SRead "17" = 17
  SRead "18" = 18
  SRead "19" = 19
  SRead "20" = 20
  SRead "21" = 21
  SRead "22" = 22
  SRead "23" = 23
  SRead "24" = 24
  SRead "25" = 25
  SRead "26" = 26
  SRead "27" = 27
  SRead "28" = 28
  SRead "29" = 29
  SRead "30" = 30
  SRead "31" = 31
  SRead "32" = 32
  SRead "33" = 33
  SRead "34" = 34
  SRead "35" = 35
  SRead "36" = 36
  SRead "37" = 37
  SRead "38" = 38
  SRead "39" = 39
  SRead "40" = 40
  SRead "41" = 41
  SRead "42" = 42
  SRead "43" = 43
  SRead "44" = 44
  SRead "45" = 45
  SRead "46" = 46
  SRead "47" = 47
  SRead "48" = 48
  SRead "49" = 49
  SRead "50" = 50
  SRead "51" = 51
  SRead "52" = 52
  SRead "53" = 53
  SRead "54" = 54
  SRead "55" = 55
  -- if you want to use higher indices, call it (SNat @n) instead of #n

instance (SRead n ~ n1, KnownNat n1) => IsLabel n (SNat n1) where
  fromLabel = SNat @n1
