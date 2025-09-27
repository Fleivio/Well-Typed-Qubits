{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module Main(main) where

import Quant
import Control.Monad 
import Data.Proxy (Proxy(..))

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

    mem <- mkQ [(read a :> read b :> read carry :> 0 :> VNil, 1)]
    putStrLn "\nPerformin quantum operations..."
    putStrLn "|x y result carryOut>"
    runQ (adder >> sample) mem

------------------------------------------------------------------

deutsch :: QBitAct 2 a -> QBitAct 2 Bit
deutsch uf = do
  app [qb|2|] x
  sample
  _ <- mapp [qb|1 2|] h
  sample
  _ <- app [qb|1 2|] uf
  sample
  app [qb|1|] h
  sample
  measure #1

testDeutsch :: (Bool -> Bool) -> IO ()
testDeutsch f = do
    putStrLn "\n\n----Deutsch test----"
    mem <- [mkq|0 0|]
    let orac = oracle (\[vec|k|] -> f $ toBool k) [qb|1|] [qb|2|]
    r <- runQ (deutsch orac) mem
    case r of
      0 -> print "is constant"
      1 -> print "is balanced"

------------------------------------------------------------------

teleport :: QBitAct 3 ()
teleport = do
  app [qb|1|] x
  app [qb|2|] h
  app [qb|2 3|] cnot
  app [qb|1 2|] cnot
  app [qb|1|] h
  [vec|control1 control2|] <- measureNBool [qb|1 2|]
  when control1 $ app [qb|3|] x
  when control2 $ app [qb|3|] z

------------------------------------------------------------------

-- In progress

zAny :: forall n. KnownNat n => QBitAct n ()
zAny = phaseOracle ([vec|n*1|] /=)

zAny3 :: QBitAct 3 ()
zAny3 = zAny @3

zAnyTest :: IO ()
zAnyTest = do 
  putStrLn "\n\n----ZAny test----"
  runQ (zAny >> sample) =<< [mkq|0 1 1|]


grover3 :: QBitAct 3 () -> QBitAct 3 (Vec 3 Bit)
grover3 zf = do
  let targets = [qb|1 2 3|]

  appAll_ h
  replicateM_ 2 ( 
    zf >> appAll_ h >> zAny >> appAll_ h
    )
  measureN targets

testGrover :: IO ()
testGrover = do 
  putStrLn "\n\n----Grover test----"
  outcome <- [mkq|0 0 0|] >>= runQ (grover3 $ phaseOracle ( == [vec|0 1 0|] )) 
  print outcome

-- Grovern -------

-- zAnyn :: KnownNat n => QBitAct n ()
-- zAnyn = phaseOracle ([vec|n*0|] /=)




-- test :: forall n. KnownNat n => Vec n
-- test = [vec|n*1|]


-- grovern :: KnownNat n => QBitAct n () -> QBitAct n ()
-- grovern zf = do
--   appAll_ h

--   replicateM_ 2 ( 
--     zf >> appAll_ h >> zAnyn >> appAll_ h
--     )
--   measureN targets
--   return ()

------------------------------------------------------------------

myOracle :: QBitAct 3 ()
myOracle = oracle (\[vec|a b|] -> a == b) [qb|1 3|] [qb|2|]

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
    -- testGrover
    -- testSqrtNot
    testDeutsch (const True)
    -- testAdder
