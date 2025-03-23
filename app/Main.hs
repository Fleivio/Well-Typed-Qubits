module Main(main) where

import Quant
import Control.Monad

sqrtNot :: QBitAct 1 ()
sqrtNot = h >> s >> h

testSqrtNot :: IO ()
testSqrtNot = do
    putStrLn "\n\n----Test sqrtNot----"
    mem <- mkQ [(O:>NNil, 1)]
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

    mem <- mkQ [(read a :> read b :> read carry :> O :> NNil, 1)]
    putStrLn "\nPerformin quantum operations..."
    putStrLn "|x y result carryOut>"
    runQ (adder >> sample) mem

------------------------------------------------------------------

deutsch :: QBitAct 2 a -> QBitAct 2 a
deutsch uf = do
  app [qb|2|]    x
  mapp [qb|1 2|] h
  eff <- app [qb|1 2|] uf
  app [qb|1|]    h
  (val:>NNil) <- measureN [qb|1|]
  case val of
    O -> liftIO $ print "f is constant" >> return eff
    I -> liftIO $ print "f is balanced" >> return eff

testDeutsch :: IO ()
testDeutsch = do
    putStrLn "\n\n----Deutsch test----"
    mem <- mkQ [(O:>O:>NNil, 1)]
    putStrLn "i. CNot:"
    runQ (deutsch cnot) mem

    mem2 <- mkQ [(O:>O:>NNil, 1)]
    putStrLn "ii. Const O:"
    runQ (deutsch $ app [qb|2|] (toState O)) mem2
    return ()

------------------------------------------------------------------

teleport :: QBitAct 3 ()
teleport = do
  app [qb|1|] x
  app [qb|2|] h
  app [qb|2 3|] cnot
  app [qb|1 2|] cnot
  app [qb|1|] h
  (control1:>control2:>NNil) <- measureNBool [qb|1 2|]
  when control1 $ app [qb|3|] x
  when control2 $ app [qb|3|] z

------------------------------------------------------------------

-- In progress

zAny :: QBitAct 3 ()
zAny = qActMatrix [
      ((O:>O:>O:>NNil, O:>O:>O:>NNil), 1),
      ((I:>O:>O:>NNil, I:>O:>O:>NNil), -1),
      ((I:>I:>O:>NNil, I:>I:>O:>NNil), -1),
      ((I:>I:>I:>NNil, I:>I:>I:>NNil), -1),
      ((O:>I:>I:>NNil, O:>I:>I:>NNil), -1),
      ((O:>O:>I:>NNil, O:>O:>I:>NNil), -1),
      ((O:>I:>O:>NNil, O:>I:>O:>NNil), -1),
      ((I:>O:>I:>NNil, I:>O:>I:>NNil), -1)
    ]

oracle111 :: QBitAct 3 ()
oracle111 = qActMatrix [
      ((O:>O:>O:>NNil, O:>O:>O:>NNil), 1),
      ((I:>O:>O:>NNil, I:>O:>O:>NNil), 1),
      ((I:>I:>O:>NNil, I:>I:>O:>NNil), 1),
      ((I:>I:>I:>NNil, I:>I:>I:>NNil), -1),
      ((O:>I:>I:>NNil, O:>I:>I:>NNil), 1),
      ((O:>O:>I:>NNil, O:>O:>I:>NNil), 1),
      ((O:>I:>O:>NNil, O:>I:>O:>NNil), 1),
      ((I:>O:>I:>NNil, I:>O:>I:>NNil), 1)
    ]

grover :: QBitAct 3 () -> QBitAct 3 ()
grover oracle = do
  let targets = [qb|1 2 3|]

  mapp targets (toState O >> h)    -- |000> 

  replicateM_ 2 ( do
    oracle
    parallel targets zAny
    )
  val <- measureN targets
  liftIO $ print val

testGrover :: IO ()
testGrover = do
  a <- mkQ [(O:>O:>O:>NNil, 1)]
  runQ (grover oracle111) a

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
