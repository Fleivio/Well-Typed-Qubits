{-# LANGUAGE LinearTypes #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main(main) where

import Quant
import Control.Monad
import Data.Proxy (Proxy(..))

-------------------------------------------------------------------

testDoubH :: IO ()
testDoubH = do
  mem <- [mkq|0|]
  runQ (h >> h >> sample) mem
  printQ mem

testEntangle :: IO ()
testEntangle = do
  putStrLn "\n\n----Test entangle----"
  mem <- [mkq|0 0|]
  runQ (app [qb|1|] h >> cnot) mem
  printQ mem

-------------------------------------------------------------------


sqrtNot :: QBitAct 1 ()
sqrtNot = h >> s >> h

testSqrtNot :: IO ()
testSqrtNot = do
    putStrLn "\n\n----Test sqrtNot----"
    mem <- [mkq|0|]
    runQ (sample >> sqrtNot >> sample >> sqrtNot >> sample) mem

------------------------------------------------------------------
adder :: QBitAct 5 ()
adder = do
  let a = #1; b = #2; sum = #3; cin = #4; cout = #5; 

  app [qb|a sum|] cnot
  app [qb|b sum|] cnot
  app [qb|a b cout|] toffoli
  app [qb|a cin cout|] toffoli
  app [qb|b cin cout|] toffoli
  app [qb|cin sum|] cnot


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

    mem <- [mkq|(read a) (read b) 0 (read carry) 0|]
    putStrLn "\nPerformin quantum operations..."
    putStrLn "|x y result carryOut>"
    runQ (adder >> sample) mem

------------------------------------------------------------------

cnot6 :: QBitAct 7 ()
cnot6 = cnotn @6

testCnot6 :: IO ()
testCnot6 = do
  putStrLn "\n\n----CNOT test----"
  m <- [mkq|0 0 0 0 0 0 0|]
  runQ ((appAll_ h ||| qid) >> cnot6) m
  printQ m

------------------------------------------------------------------

deutsch :: QBitAct 2 a -> QBitAct 2 Bit
deutsch uf = do
  app   [qb|2|]   x
  mapp_ [qb|1 2|] h
  _ <- uf
  app [qb|1|] h
  measure #1

testDeutsch :: (Bool -> Bool) -> IO ()
testDeutsch f = do
    putStrLn "\n\n----Deutsch test----"
    mem <- [mkq|0 0|]
    let orac = orc (\[vec|k _|] -> f $ toBool k)
                   (\[vec|a b|] -> [vec|a (lnegate b)|])

    r <- runQ (deutsch orac) mem
    case r of
      0 -> print "is constant"
      1 -> print "is balanced"

------------------------------------------------------------------

deutschJ :: forall n a. Partition (n-1) 1 n => QBitAct n a -> QBitAct n (Vec (n-1) Bit)
deutschJ uf = do
  app (SNat @n :- SNil) x
  -- appAll_ @(n-1) qid ||| x
  appAll_ h
  _ <- uf
  appAll_ @(n-1) h ||| qid
  (output, _) <- measureAll <||| qid
  return output

testDeutschJ :: (Bool -> Bool -> Bool) -> IO ()
testDeutschJ f = do
    putStrLn "\n\n----Deutsch test----"
    mem <- [mkq|3*0|]

    let orac = orc (\[vec|a b _|] -> f (toBool a) (toBool b))
                   (\[vec|a b r|] -> [vec|a b (lnegate r)|])

    v <- runQ (deutschJ orac) mem
    case v of
      [vec|0 0|] -> print "is constant"
      _ -> print "is balanced"

-----------------------------------------------------------------

teleport :: QBitAct 3 ()
teleport = do
  app [qb|1|] sample
  app [qb|2 3|] entangle
  app [qb|1 2|] cnot
  app [qb|1|] h

  [vec|control1 control2|] <- measureNBool [qb|1 2|]
  when control2 $ app [qb|3|] x
  when control1 $ app [qb|3|] z
  app [qb|3|] sample

testTeleport :: IO ()
testTeleport = do
  m <- [mkq|0 0 0|]
  runQ (app [qb|1|] (x >> h) >> teleport) m
  printQ m

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

-------

clone :: QBitAct 2 ()
clone = liftQ (\[vec|a b|] -> [vec|(lnegate b) a|])

main :: IO ()
main = do
  print "aa"