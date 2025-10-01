module Core.Bit(Bit(..), toBool) where

data Bit = O | I deriving (Ord, Read)

instance Num Bit where
    a + b = if a == 1 then 1 else b
    a * b = if a == 0 then 0 else b

    abs = id
    signum = id
    negate a = if a == 1 then 0 else 1
    fromInteger a = if even a then O else I

instance Show Bit where
  show O = "0"
  show I = "1"

instance Eq Bit where 
  O == O = True
  I == I = True
  _ == _ = False


toBool :: Bit -> Bool
toBool O = False
toBool I = True