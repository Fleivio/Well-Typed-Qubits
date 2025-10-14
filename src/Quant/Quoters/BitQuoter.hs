module Quoters.BitQuoter(vec, mkq, parseNList) where

import List.Vec
import Core.Virt

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.Char (isDigit)
import Data.Proxy (Proxy(..))
import Language.Haskell.Meta.Parse (parseExp)

vec :: QuasiQuoter
vec = QuasiQuoter
  { quoteExp  = parseNList
  , quotePat  = parseNListPat
  , quoteType = undefined
  , quoteDec  = undefined
  }

parseBit :: String -> Q Exp
parseBit "1" = [| 1 |]
parseBit "0" = [| 0 |]
parseBit k   =
  case parseExp k of
    Left e -> error e
    Right a -> return a

splitParenAware :: String -> [String]
splitParenAware = go 0 "" []
  where
    go _ acc toks [] =
      let acc' = dropWhile (== ' ') acc
      in if null acc' then reverse toks else reverse (acc' : toks)
    go n acc toks (x:xs)
      | x == '('  = go (n+1) (acc ++ [x]) toks xs
      | x == ')'  = go (n-1) (acc ++ [x]) toks xs
      | x == ' ' && n == 0 =
          let acc' = dropWhile (== ' ') acc
          in if null acc'
             then go n "" toks xs
             else go n "" (acc':toks) xs
      | otherwise = go n (acc ++ [x]) toks xs

parseBits :: String -> Q Exp
parseBits input = do
  let bitList = parseBit <$> splitParenAware input
  foldr (\x acc -> [|$x :> $acc|]) [|VNil|] bitList 

parseMultiplicationFactor :: String -> Q Exp -> Q Exp
parseMultiplicationFactor s ex
  | all isDigit s = let k = read s :: Int 
                    in foldr (\x xs -> [|$x :> $xs|]) [|VNil|] $ replicate k ex
  | otherwise =
    let sizeExpr = [|fromIntegral $ natVal (Proxy @($(varT $ mkName s)))|]
    in [| unsafeVec @($(varT (mkName s))) $ replicate $sizeExpr $ex |]

parseMultiplication :: String -> Q Exp
parseMultiplication inputString 
      = case break (== '*') inputString of
        (r1, '*':r2) -> parseMultiplicationFactor r1 $ parseBit r2
        _ -> error ""

parseNList :: String -> Q Exp
parseNList input
  | '*' `elem` input = parseMultiplication input
  | otherwise        = parseBits input 

parseNListPat :: String -> Q Pat
parseNListPat input = do
  let vars = words input
  foldr (\var acc -> if var `elem` ["1", "0"] 
                        then [p| $(litP $ IntegerL $ read var) :> $acc |] 
                        else [p| $(varP (mkName var)) :> $acc |]) 
        [p| VNil |] vars

---------------------------------

mkq :: QuasiQuoter
mkq = QuasiQuoter
  { quoteExp  = parseVirt
  , quotePat  = undefined
  , quoteType = undefined
  , quoteDec  = undefined
  }

parseVirt :: String -> Q Exp
parseVirt str =
    [| mkQ [ ($(parseNList str), 1) ] |]
