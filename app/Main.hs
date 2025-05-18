{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Main(main) where

import Quant
import Control.Monad 

sqrtNot :: QBitAct 1 ()
sqrtNot = h >> s >> h

testSqrtNot :: IO ()
testSqrtNot = do
    putStrLn "\n\n----Test sqrtNot----"
    mem <- [mkq|0|]
    runQ (sample >> sqrtNot >> sample >> sqrtNot >> sample) mem

------------------------------------------------------------------
adder :: QBitAct 4 ()
adder = do
  let a = #1
      b = #2
      cIn = #3
      cOut = #4

  app [qb|a b cOut|]   toffoli
  app [qb|a b|]        cnot
  app [qb|b cIn cOut|] toffoli
  app [qb|b cIn|]      cnot
  app [qb|a b|]        cnot

testAdder :: IO ()
testAdder = do
    putStrLn "\n\n----Test adder----"
    putStrLn "Enter numbers to sum"
    putStrLn "x: "
    a <- getLine    
    putStrLn "y: "
    b <- getLine
    putStrLn "carryIn: "
    carry <- getLine

    mem <- mkQ [(read a :> read b :> read carry :> 0 :> NNil, 1)]
    putStrLn "\nPerformin quantum operations..."
    putStrLn "|x y result carryOut>"
    runQ (adder >> sample) mem

------------------------------------------------------------------

deutsch :: QBitAct 2 a -> QBitAct 2 Bit
deutsch uf = do
  app [qb|2|]    x
  _ <- mapp [qb|1 2|] h
  _ <- app [qb|1 2|] uf
  app [qb|1|] h
  measure #1

testDeutsch :: IO ()
testDeutsch = do
    putStrLn "\n\n----Deutsch test----"
    mem <- [mkq|0 0|]
    putStrLn "i. CNot:"
    r <- runQ (deutsch cnot) mem
    case r of
      0 -> print "cnot is constant"
      1 -> print "cnot is balanced"

------------------------------------------------------------------

teleport :: QBitAct 3 ()
teleport = do
  app [qb|1|] x
  app [qb|2|] h
  app [qb|2 3|] cnot
  app [qb|1 2|] cnot
  app [qb|1|] h
  [nl|control1 control2|] <- measureNBool [qb|1 2|]
  when control1 $ app [qb|3|] x
  when control2 $ app [qb|3|] z

------------------------------------------------------------------

-- In progress

toBool :: Bit -> Bool
toBool 0 = False
toBool 1 = True

zAny :: QBitAct 3 ()
zAny = phaseOracle (not . toBool . sum)

grover :: QBitAct 3 () -> QBitAct 3 (Vec 3 Bit)
grover zf = do
  let targets = [qb|1 2 3|]
  
  appAll_ (toState 0 >> h)

  replicateM_ 2 ( 
    zf >> parallel zAny
    )
  measureN targets

testGrover :: IO ()
testGrover = do 
  putStrLn "\n\n----Grover test----"
  outcome <- [mkq|0 0 0|] >>= runQ (grover $ phaseOracle ( == [nl|0 1 0|] )) 
  print outcome

------------------------------------------------------------------

myOracle :: QBitAct 3 ()
myOracle = oracle (\[nl|a b|] -> a == b) [qb|1 3|] [qb|2|]

testOracle :: IO ()
testOracle = do
  putStrLn "\n\n----Oracle test----"
  mem <- [mkq|0 0 0|]
  runQ (mapp [qb|1 3|] h >> myOracle) mem
  printQ mem

------------------------------------------------------------------
  
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



main :: IO ()
main = do
    testGrover
    -- testSqrtNot
    -- testDeutsch
    -- testAdder
    -- testOracle
