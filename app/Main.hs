module Main(main) where

import Deutsch
import Grover
import Teleport

main :: IO ()
main = do 
  runDeutsch (\_ _ -> True)
  runDeutsch (\_ _ -> False)
  runGrover
  runTeleport
