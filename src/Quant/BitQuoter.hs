module BitQuoter(vec, mkq) where

import List.Vec
import Core.Virt

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.Char (isDigit)
import Data.Proxy (Proxy(..))


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
parseBit _   = error "Bit should be 1 or 0"

parseBits :: String -> Q Exp
parseBits input = do
  let bitList = parseBit <$> words input
  foldr (\x acc -> [|$x :> $acc|]) [|VNil|] bitList 

parseMultiplicationFactor :: String -> Q Exp -> Q Exp
parseMultiplicationFactor s ex
  | all isDigit s = let k = read s :: Int 
                    in foldr (\x xs -> [|$x :> $xs|]) [|VNil|] $ replicate k ex
  | otherwise =
    let sizeExpr = pure $
                AppE (VarE (mkName "fromIntegral")) 
                (AppE (VarE (mkName "natVal")) (ConE (mkName "Proxy") `AppTypeE` VarT (mkName s)))
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
  foldr (\var acc ->  [p| $(varP (mkName var)) :> $acc |]) [p| VNil |] vars

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
