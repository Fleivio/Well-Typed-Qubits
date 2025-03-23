module Core.Bit(Bit(..)) where

data Bit = O | I deriving (Ord, Read)

instance Show Bit where
  show O = "0"
  show I = "1"

instance Eq Bit where 
  O == O = True
  I == I = True
  _ == _ = False