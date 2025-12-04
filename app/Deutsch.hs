module Deutsch(runDeutsch) where

import Quant

deutschJ :: forall n a. Partition (n-1) 1 n => QBitAct n a -> QBitAct n (Vec (n-1) Bit)
deutschJ uf = do
  appAll qid ||| x
  appAll_ h
  _ <- uf
  appAll_ h ||| qid
  (output, _) <- measureAll <||| qid
  return outpu

runDeutsch :: (Bool -> Bool -> Bool) -> IO ()
runDeutsch f = do
    putStrLn "\n\n----Deutsch test----"
    mem <- [mkq|3*0|]

    let orac = orc (\[vec|a b _|] -> toBool a `f` toBool b)
                   (\[vec|a b r|] -> [vec|a b (lnegate r)|])

    v <- runQ (deutschJ orac) mem
    case v of
      [vec|0 0|] -> print "The function is constant" 
      _ -> print "The function is balanced" 