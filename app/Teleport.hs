module Teleport(runTeleport) where

import Quant
import Control.Monad (when)

teleport :: QBitAct 3 ()
teleport = do
  liftIO $ print "Initial state (qubit-1)"
  app [qb|1|] sample
  app [qb|2 3|] entangle
  app [qb|1 2|] cnot
  app [qb|1|] h

  [vec|control1 control2|] <- measureNBool [qb|1 2|]
  when control2 $ app [qb|3|] x
  when control1 $ app [qb|3|] z
  liftIO $ print "Received state (qubit-3)"
  app [qb|3|] sample

runTeleport :: IO ()
runTeleport = do
  m <- [mkq|0 0 0|]
  putStrLn "\n\n----Teleport test----"
  runQ (app [qb|1|] (x >> h) >> teleport) m
  printQ m
