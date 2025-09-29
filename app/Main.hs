{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeFamilies, AllowAmbiguousTypes #-}
module Main(main) where

import Quant
import Control.Monad
import Data.Proxy (Proxy(..))
import GHC.TypeLits

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


main :: IO ()
main = do
  testGrover