module Test(testNoMonad, entangleExample) where

import Core.Virt
import List.SList
import List.NList
import List.OvLabel

h = mkOP [(([O],[O]),1),(([O],[I]),1),(([I],[O]),1),(([I],[I]),-1)]
cnot = mkOP [(([O,O],[O,O]),1),(([O,I],[O,I]),1),(([I,O],[I,I]),1),(([I,I],[I,O]),1)]

safeSelectQ :: ValidSelector nacs n => SList nacs -> Virt a n -> Virt a (Length nacs)
safeSelectQ = selectQ

testNoMonad :: IO ()
testNoMonad = do
  value <- mkQ [(I:>O:>I:>O:>NNil, 1)]
  let v1 = safeSelectQ [qb|2|] value
  appV h v1
  printQ value

entangleExample :: IO ()
entangleExample = do
  value <- mkQ [(O:>O:>NNil, 1)]
  let v1 = safeSelectQ [qb|1|] value
  appV h v1
  appV cnot value
  printQ value