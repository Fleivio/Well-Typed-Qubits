module Quoters.MatrixQuoter(
  matrix) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.Meta.Parse (parseExp)

import Quoters.BitQuoter (vec, parseNList)
import QAct.QAct (qActMatrix)
import Core.PA (squareModulus)


matrix :: QuasiQuoter
matrix = QuasiQuoter
  { quoteExp  = parseMatrix
  , quotePat  = undefined
  , quoteType = undefined
  , quoteDec  = undefined
  }

isPower2 :: Int -> Bool
isPower2 = ((==) <*> (fromInteger . round)) . logBase 2 . fromIntegral

parseMatrix :: String -> Q Exp
parseMatrix input
  | isPower2 (length validLines) && length validLines > 1 = parseEntries validLines
  | otherwise = error "Number of lines must be a power of two"
  where
    validLines = filter (not . all (`elem` [' ', '\t'])) $ filter (not.null) $ lines input

parseEntries :: [String] -> Q Exp
parseEntries inputs = [|qActMatrix $buildindList |]
  where
    rowMatch row
      = case break (== '=') row of
          (entry, '=':'[':rest)
            -> case break (== ']') rest of
               (delta, ']':'=':'>':out) -> (entry, delta, out)
               _                        -> error "syntax error on matrix entry"
          _ -> error "syntax error on matrix entry"
    rows = rowMatch <$> inputs
    
    -- parsing
    (entries, deltas, outs) = unzip3 rows

    parsedEntries = parseNList <$> entries
    parsedOuts = parseNList <$> outs
    parsedDeltas =
      (\s -> case parseExp s of
        Left err -> error err
        Right e  -> return @Q e) <$> deltas

    tupleEntries = zip3 parsedEntries parsedOuts parsedDeltas
    buildindList = foldr (\(e,o,d) acc -> [| (($e, $o), $d) : $acc |]) [|[]|] tupleEntries

{-

fredkin :: QBitAct 3 ()
fredkin = qActMatrix [
      (([vec|0 0 0|], [vec|0 0 0|]), 1),
      (([vec|0 0 1|], [vec|0 0 1|]), 1),
      (([vec|0 1 0|], [vec|0 1 0|]), 1),
      (([vec|0 1 1|], [vec|0 1 1|]), 1),
      (([vec|1 0 0|], [vec|1 0 0|]), 1),
      (([vec|1 0 1|], [vec|1 1 0|]), 1),
      (([vec|1 1 0|], [vec|1 0 1|]), 1),
      (([vec|1 1 1|], [vec|1 1 1|]), 1)
    ]

fredkin :: QBitAct 3 ()
fredkin = [matrix|
0 0 0 =[1]=> 0 0 0

      (([vec|0 0 0|], [vec|0 0 0|]), 1),
      (([vec|0 0 1|], [vec|0 0 1|]), 1),
      (([vec|0 1 0|], [vec|0 1 0|]), 1),
      (([vec|0 1 1|], [vec|0 1 1|]), 1),
      (([vec|1 0 0|], [vec|1 0 0|]), 1),
      (([vec|1 0 1|], [vec|1 1 0|]), 1),
      (([vec|1 1 0|], [vec|1 0 1|]), 1),
      (([vec|1 1 1|], [vec|1 1 1|]), 1)
    |]
-}