{-# OPTIONS_GHC -Wno-dodgy-exports #-}
module Quant(module QAct.QBitAct, module QAct.QAct, module List.OvLabel, module List.NList, module BitQuoter) where

import QAct.QBitAct
import QAct.QAct
import List.OvLabel ()
import List.NList
import BitQuoter

-- haddamardTest :: IO ()
-- haddamardTest = do
--   print "-----------------"
--   val <- mkQ [(I:>O:>O:>O:>NNil, 1)]

--   runQ
--     (do
--       app (#1 :- SNil) h
--       app (#3 :- SNil) h)
--     val
--   printQ val

-- hiperEntangle :: QBitAct 3 ()
-- hiperEntangle = do
--   app (SNat @1 :- SNat @2 :- SNil) entangle
--   sample
--   app (SNat @2 :- SNat @3 :- SNil) entangle
--   sample

-- addTest :: IO ()
-- addTest = do 
--   print "-----------------"
--   val <- mkQ [(I:>O:>O:>O:>NNil, 1)]
--   runQ adder val
--   printQ val