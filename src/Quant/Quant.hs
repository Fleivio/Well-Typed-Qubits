module Quant(a) where

import QAct.QBitAct
import Demos

a :: IO ()
a = do 
  print "-----------------"
  val <- mkQ [(O:>O:>O:>O:>O:>NNil, 1)]
  runQ (mapp [qb|1 3 5|] h) val
  printQ val

  print "-----------------"
  val <- mkQ [(O:>O:>O:>O:>NNil, 1)]
  runQ (mapp (upTo #3) h) val
  printQ val

test :: (ValidSelector (Count k) n, KnownNat k) => SNat k -> QBitAct n ()
test k = do
  (mapp (upTo k) h)