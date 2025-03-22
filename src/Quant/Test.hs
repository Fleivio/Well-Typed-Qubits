module Test(testNoMonad, entangleExample, adderExample) where

import Core.Virt
import List.SList
import List.NList
import List.OvLabel

h :: OP Bit
h = mkOP [(([O],[O]),1),(([O],[I]),1),(([I],[O]),1),(([I],[I]),-1)]
cnot :: OP Bit
cnot = mkOP [(([O,O],[O,O]),1),(([O,I],[O,I]),1),(([I,O],[I,I]),1),(([I,I],[I,O]),1)]
toffoli :: OP Bit
toffoli = mkOP [(([O,O,O],[O,O,O]),1),
                (([O,O,I],[O,O,I]),1),
                (([O,I,O],[O,I,O]),1),
                (([O,I,I],[O,I,I]),1),
                (([I,O,I],[I,O,I]),1),
                (([I,O,O],[I,O,O]),1),
                (([I,I,O],[I,I,I]),1),
                (([I,I,I],[I,I,O]),1)]

selectQ :: ValidSelector nacs n => SList nacs -> Virt a n -> Virt a (Length nacs)
selectQ = unsafeSelectQ

testNoMonad :: IO ()
testNoMonad = do
  value <- mkQ [(I:>O:>I:>O:>NNil, 1)]
  let v1 = selectQ [qb|2|] value
  appV h v1
  printQ value

entangleExample :: IO ()
entangleExample = do
  value <- mkQ [(O:>O:>NNil, 1)]
  let v1 = selectQ [qb|1|] value
  appV h v1
  appV cnot value
  printQ value

adderExample :: IO ()
adderExample = do
  mem <- mkQ [(I:>O:>O:>O:>NNil, 1)]

  let
    q_124 = selectQ (SNat @1 :- SNat @2 :- SNat @4 :- SNil) mem
    q_12 = selectQ (SNat @1 :- SNat @2 :- SNil) mem
    q_234 = selectQ (SNat @2 :- SNat @3 :- SNat @4 :- SNil) mem
    q_23 = selectQ (SNat @2 :- SNat @3 :- SNil) mem

  appV toffoli q_124
  appV cnot q_12
  appV toffoli q_234
  appV cnot q_23      
  appV cnot q_12
  printQ mem       