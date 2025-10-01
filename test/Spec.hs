module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import Quant

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Meu-Projeto Tests"
  [ testCase "appAll_ is equivalent to mapp_" mappEqualsAppAll,
    testCase "mapp builds vector with correct length" mappVectorBuild,
    testCase "parallel app is equivalent to sequential" parallelAndSeq
  ]

-----------------------------------------------

mappEqualsAppAll :: IO ()
mappEqualsAppAll = do
  mem1 <- [mkq|1 0 0|]
  _ <- runQ (appAll_ h) mem1

  mem2 <- [mkq|1 0 0|]
  _ <- runQ (mapp_ [qb|1 2 3|] h) mem2
  
  qv1 <- getQV mem1
  qv2 <- getQV mem2

  if qv1 == qv2 then return () else error ""

mappVectorBuild :: IO ()
mappVectorBuild = do
  mem <- [mkq|1 0 0|] :: IO (Virt Bit 3)
  v@[vec|a b c|] <- runQ (appAll (return 1)) mem

  if vecToList v == [a,b,c] then return () else error ""

parallelAndSeq :: IO ()
parallelAndSeq = do
  mem1 <- [mkq|1 0 0|] 
  mem2 <- [mkq|1 0 0|]

  _ <- runQ (cnot ||| h) mem1 
  _ <- runQ (app [qb|1 2|] cnot >> app [qb|3|] h) mem2

  qv1 <- getQV mem1
  qv2 <- getQV mem2
  if qv1 == qv2 then return () else error ""

