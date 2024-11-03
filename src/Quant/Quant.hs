module Quant(a) where

import QAct.QBitAct
import Demos

a :: IO ()
a = do 
  val <- mkQ [((I:>O:>I:>O:>NNil), 1)]
  runQ adder val
  printQ val