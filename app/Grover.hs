module Grover(runGrover) where

import Quant
import Control.Monad (replicateM_)
import Data.Proxy

data Color = Red | Blue | Green | Yellow deriving (Eq, Show)
type Graph = [(Int,Int)]

vecToColors :: Vec n Bit -> [Color]
vecToColors (a:>b:>rest) =
  (case (a,b) of
    (0,0) -> Red
    (0,1) -> Blue
    (1,0) -> Green
    (1,1) -> Yellow) : vecToColors rest
vecToColors _ = []

colorOracle :: Graph -> Vec n Bit -> Bool
colorOracle edgs bits = 
  let colors = vecToColors bits
  in all (\(u,v) -> colors !! u /= colors !! v) edgs

graph :: Graph
graph = [(0,2), (0,1), (2,3), (1,3), (1,2)]

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
    it = floor $ (pi/4 :: Double) * sqrt (2 ** (fromIntegral.natVal $ Proxy @n) / fromIntegral slCounter )

runGrover :: IO ()
runGrover = do
  putStrLn "\n\n----Grover graph----"
  mem <- [mkq|0 0 0 0 0 0 0 0|] 
  outcome <- runQ (grover 48 $ phaseOracle (colorOracle graph)) mem
  print $ vecToColors outcome

