module Core.PA
  ( PA
  , showPA
  , showPAMultiplicative
  , squareModulus
  , module Data.Complex
  ) where

import Data.Complex (Complex (..), imagPart, magnitude, realPart)
import Text.Printf

type PA = Complex Double

showPA :: PA -> String
showPA pa = realp <> connect <> imagp
  where 
    connect
      | null realp || null imagp = ""
      | imagPart pa < 0 = "-"
      | otherwise = "+"
    realp
      | realPart pa == 0 = ""
      | otherwise = printf "%.3f" (realPart pa)
    imagp
      | imagPart pa == 0 = ""
      | otherwise        = printf "%.3f_i" (imagPart pa)

showPAMultiplicative :: PA -> String
showPAMultiplicative pa =
  case pa of
    1   -> mempty
    (-1) -> "-"
    a   -> showPA a

squareModulus :: PA -> Double
squareModulus = (** 2) . magnitude
