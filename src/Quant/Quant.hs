module Quant(a) where

import QAct.QBitAct
import Demos

test :: QBitAct 3 ()
test = do
  app (#1 :- SNil) h
  app (#3 :- SNil) h
  sample
  _ <- measure #1
  return ()

a :: IO ()
a = do 
  print "-----------------"
  val <- mkQ [(O:>O:>O:>NNil, 1)]
  (v, h) <- runQHist test val
  printQ val
  print v
  print h