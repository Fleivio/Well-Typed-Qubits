module Main(main) where

import Quant
import Control.Monad

sqrtNot :: QBitAct 1 ()
sqrtNot = h >> s >> h

testSqrtNot :: IO ()
testSqrtNot = do
    putStrLn "\n\n----Test sqrtNot----"
    mem <- [mkq|O|]
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

deutsch :: QBitAct 2 a -> QBitAct 2 Bit
deutsch uf = do
  app [qb|2|]    x
  _ <- mapp [qb|1 2|] h
  _ <- app [qb|1 2|] uf
  app [qb|1|]    h
  measure #1

testDeutsch :: IO ()
testDeutsch = do
    putStrLn "\n\n----Deutsch test----"
    mem <- [mkq|O O|]
    putStrLn "i. CNot:"
    r <- runQ (deutsch cnot) mem
    case r of
      O -> print "cnot is constant"
      I -> print "cnot is balanced"


    mem2 <- [mkq|O O|]
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
  [nl|control1 control2|] <- measureNBool [qb|1 2|]
  when control1 $ app [qb|3|] x
  when control2 $ app [qb|3|] z

------------------------------------------------------------------

-- In progress

zAny :: QBitAct 3 ()
zAny = phaseOracle (not . (==) [nl|O O O|])

grover :: QBitAct 3 () -> QBitAct 3 ()
grover zf = do
  let targets = [qb|1 2 3|]

  mapp targets (toState O >> h)    -- |000> 

  replicateM_ 2 ( do
    zf
    parallel targets zAny
    )
  val <- measureN targets
  liftIO $ print val

testGrover :: IO ()
testGrover = do 
  putStrLn "\n\n----Grover test----"
  [mkq|O O O|] >>= runQ (grover $ phaseOracle ( == [nl|I O I|] ))

------------------------------------------------------------------

myOracle :: QBitAct 3 ()
myOracle = oracle (\[nl|a b|] -> a == b) [qb|1 3|] [qb|2|]

testOracle :: IO ()
testOracle = do
  putStrLn "\n\n----Oracle test----"
  mem <- [mkq|O O O|]
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
    testOracle
