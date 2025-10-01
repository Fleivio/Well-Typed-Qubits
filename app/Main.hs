{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeFamilies, AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Main(main) where

import Quant
import Control.Monad
import Data.Proxy (Proxy(..))

-------------------------------------------------------------------

testDoubH :: IO ()
testDoubH = do
  mem <- [mkq|0|]
  runQ (h >> h >> sample) mem

-------------------------------------------------------------------


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
  app [qb|1 2 4|]   toffoli
  app [qb|1 2|]        cnot
  app [qb|2 3 4|] toffoli
  app [qb|2 3|]      cnot
  app [qb|1 2|]        cnot

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
  _ <- mapp [qb|1 2|] h
  _ <- uf
  app [qb|1|] h
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

deutschJ :: forall n a. Partition (n-1) 1 n => QBitAct n a -> QBitAct n (Vec (n-1) Bit)
deutschJ uf = do
  appAll_ @(n-1) qid ||| x
  appAll_ h
  _ <- uf
  appAll_ @(n-1) h ||| qid
  (output, _) <- measureAll <||| qid
  return output

testDeutschJ :: (Bool -> Bool) -> IO ()
testDeutschJ f = do
    putStrLn "\n\n----Deutsch test----"
    mem <- [mkq|0 0|]
    let orac = oracle (\[vec|k|] -> f $ toBool k) [qb|1|] [qb|2|]
    [vec|r|] <- runQ (deutschJ orac) mem
    case r of
      0 -> print "is constant"
      1 -> print "is balanced"

-----------------------------------------------------------------

teleport :: QBitAct 3 ()
teleport = do
  x ||| h ||| qid
  app [qb|2 3|] cnot
  app [qb|1 2|] cnot
  app [qb|1|] h
  [vec|control1 control2|] <- measureNBool [qb|1 2|]
  when control1 $ app [qb|3|] x
  when control2 $ app [qb|3|] z

------------------------------------------------------------------

zAny :: forall n. KnownNat n => QBitAct n ()
zAny = phaseOracle ([vec|n*0|] /=)

grover :: forall n. KnownNat n => Int -> QBitAct n () -> QBitAct n (Vec n Bit)
grover slCounter zf = do
  appAll_ h
  replicateM_ it (
    zf >> appAll_ h >> zAny @n >> appAll_ h
    )
  measureAll
  where
    it = ceiling $ (pi/4 :: Double) * sqrt((fromIntegral . natVal $ Proxy @n) / fromIntegral slCounter )  

testGrover :: IO ()
testGrover = do
  putStrLn "\n\n----Grover test----"
  outcome <- [mkq|0 0 0|] >>= runQ (grover 1 $ phaseOracle ( == [vec|0 1 0|] ))
  print outcome

------------------------------------------------------------------

myOracle :: QBitAct 3 ()
myOracle = oracle (\[vec|a b|] -> a == b) [qb|1 3|] [qb|2|]

testOracle :: IO ()
testOracle = do
  putStrLn "\n\n----Oracle test----"
  mem <- [mkq|0 0 0|]
  runQ (mapp [qb|1 3|] h >> myOracle) mem
  printQ mem

-------------------------------------------------------------------

test :: forall n. Partition (n-1) 1 n => QBitAct (n-1) () -> QBitAct n ()
test qq = do
  qq ||| h
  return ()

test2 :: forall n m k. Partition m k n 
  => QBitAct m () -> QBitAct k () -> QBitAct n ()
test2 mm kk = do
  mm ||| kk
  return ()

test3 :: forall n. Partition (n-1) 1 n => QBitAct n ()
test3 = do 
  appAll_ qid ||| h

runTest3 :: IO ()
runTest3 = do
  mem <- [mkq|0 0 0|]
  runQ test3 mem
  printQ mem

test4 :: forall n m k. Partition k m n => QBitAct k () -> QBitAct m () -> QBitAct n ()
test4 p1 p2 = do 
  p1 ||| p2
  sample

runTest4 :: IO ()
runTest4 = do
  mem <- [mkq|1 0 0|]
  runQ (test4 cnot h) mem

---------------------------------------

debugF :: QBitAct 2 ()
debugF = do
  sample
  liftIO $ print "que coisa"
  _ <- measureAll
  return ()

myControlled :: QBitAct 3 ()
myControlled = controlledAct (\[vec|c|] -> c == 1) [qb|1|] [qb|2 3|] cnot

testControlled :: IO ()
testControlled = do
  mem <- [mkq|0 0 1|]
  runQ (appAll_ h) mem 

  runQ myControlled mem
  printQ mem

main :: IO ()
main = do
  testControlled