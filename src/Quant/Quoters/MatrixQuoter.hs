module Quoters.MatrixQuoter(
  matrix) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.Meta.Parse (parseExp)

import Quoters.BitQuoter (parseNList)

matrix :: QuasiQuoter
matrix = QuasiQuoter
  { quoteExp  = parseMatrix
  , quotePat  = undefined
  , quoteType = undefined
  , quoteDec  = undefined
  }

isPower2 :: Int -> Bool
isPower2 = ((==) @Double <*> (fromInteger . round)) . logBase 2 . fromIntegral

parseMatrix :: String -> Q Exp
parseMatrix input
  | isPower2 (length validLines) && length validLines > 1 = parseEntries validLines
  | otherwise = error "Number of lines must be a power of two"
  where
    validLines = filter (not . all (`elem` [' ', '\t'])) $ filter (not.null) $ lines input

parseEntries :: [String] -> Q Exp
parseEntries inputs = [|$buildindList |]
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