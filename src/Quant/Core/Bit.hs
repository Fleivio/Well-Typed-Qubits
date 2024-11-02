module Core.Bit(Bit(..)) where

data Bit = O | I deriving (Eq, Ord)

instance Show Bit where
  show O = "0"
  show I = "1"