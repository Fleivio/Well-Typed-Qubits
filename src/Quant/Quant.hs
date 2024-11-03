module Quant(a) where

import QAct.QBitAct
import Demos
import List.Quoter

test :: QBitAct 3 ()
test = do
  sample
  app [qb|1 2|] entangle
  sample
  b <- measure #1
  liftIO $ print b
  sample

a :: IO ()
a = do 
  val <- mkQ [(O:>O:>O:>NNil, 1), (I:>O:>O:>NNil, 1)]
  runQ teleport val
  printQ val