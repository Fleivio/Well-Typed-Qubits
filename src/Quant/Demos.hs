module Demos(adder, deutsch, teleport) where

import QAct.QBitAct
import List.OvLabel ()

adder :: QBitAct 4 ()
adder = do
  app [qb|1 2 4|] toffoli
  app [qb|1 2|]   cnot
  app [qb|2 3 4|] toffoli
  app [qb|2 3|]   cnot
  app [qb|1 2|]   cnot

deutsch :: QBitAct 2 () -> QBitAct 2 ()
deutsch uf = do
  app [qb|1|]   h
  app [qb|2|]   h
  app [qb|1 2|] uf
  app [qb|1|]   h
  val <- measure #1
  case val of
    O -> liftIO $ print "f is constant"
    I -> liftIO $ print "f is balanced"

teleport :: QBitAct 3 ()
teleport = do
  app (#2 :- #3 :- SNil) entangle
  app (#1 :- #2 :- SNil) cnot
  app (#1 :- SNil) h
  _ <- measure #1
  _ <- measure #2
  app (#2 :- #3 :- SNil) cnot
  app (#1 :- #3 :- SNil) cz

-- grover :: forall n. (KnwonNat n, 2 <= n) => QBitAct n ()

{-
errorExample :: QBitAct 4 ()
errorExample = do
  app [qb|1 2 3|] toffoli --fine
  app [qb|1 2 5|] toffoli
  {- Index out of bounds on Qubit selection
       You got 4 qubits
       But tried to select qubits [1, 2, 5] -}
  app [qb|1|] cnot
  {- Couldn't match type ‘2’ with ‘1’
      Expected: QAct Bit (Length '[1]) ()
        Actual: QBitAct 2 () -}
  app [qb|0|] h
  {- Zero qubit selection is not allowed
      The qubit selection list starts from 1 -}
  app [qb|1 1|] cnot
  {- No Cloning Theorem Violation
      You tried to select qubits with repetition [1, 1] -}
-}
