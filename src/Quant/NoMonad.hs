module NoMonad(testNoMonad, entangleExample, adderExample) where

import Core.Virt
import List.SList
import List.Vec
import List.OvLabel

h :: OP Bit
h = mkOP [(([0],[0]),1),(([0],[1]),1),(([1],[0]),1),(([1],[1]),-1)]
cnot :: OP Bit
cnot = mkOP [(([0,0],[0,0]),1),(([0,1],[0,1]),1),(([1,0],[1,1]),1),(([1,1],[1,0]),1)]
toffoli :: OP Bit
toffoli = mkOP [(([0,0,0],[0,0,0]),1),
                (([0,0,1],[0,0,1]),1),
                (([0,1,0],[0,1,0]),1),
                (([0,1,1],[0,1,1]),1),
                (([1,0,1],[1,0,1]),1),
                (([1,0,0],[1,0,0]),1),
                (([1,1,0],[1,1,1]),1),
                (([1,1,1],[1,1,0]),1)]

selectQ :: ValidSelector nacs n => SList nacs -> Virt a n -> Virt a (Length nacs)
selectQ = unsafeSelectQ

testNoMonad :: IO ()
testNoMonad = do
  value <- mkQ [(1:>0:>1:>0:>NNil, 1)]
  let v1 = selectQ [qb|2|] value
  appV h v1
  printQ value

entangleExample :: IO ()
entangleExample = do
  value <- mkQ [(0:>0:>NNil, 1)]
  let v1 = selectQ [qb|1|] value
  appV h v1
  appV cnot value
  printQ value

adderExample :: IO ()
adderExample = do
  mem <- mkQ [(1:>0:>0:>0:>NNil, 1)]

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