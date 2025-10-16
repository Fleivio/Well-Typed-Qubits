module Quoters.SListQuoter(qb) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.Char (isNumber, isDigit)

qb :: QuasiQuoter
qb = QuasiQuoter
  { quoteExp  = slistExp
  , quotePat  = undefined
  , quoteType = undefined
  , quoteDec  = undefined
  }

slistExp :: String -> Q Exp
slistExp input
  | '.' `elem` input = intervalGenerator input
  | otherwise        = parseInts $ words input

intervalGenerator :: String -> Q Exp
intervalGenerator str 
  = case break (== '.') str of
    (lb, '.':ub) 
      | all isDigit lb && all isDigit ub -> pure $ VarE (mkName "sListRange") `AppTypeE` LitT (NumTyLit (read lb))  `AppTypeE` LitT (NumTyLit (read ub))
      | otherwise -> pure $ VarE (mkName "sListRange") `AppTypeE` LitT (NumTyLit (read lb))  `AppTypeE` VarT (mkName ub)
    _ -> error "Invalid range syntax selection over qubits"

parseInts :: [String] -> Q Exp
parseInts []     = [| SNil |]
parseInts (x:xs)
  | all isNumber x = do
    let n = read x :: Integer
    [| SNat @($(litT (numTyLit n))) :- $(parseInts xs) |]
  | head x == '@' = [| SNat @($(varT $ mkName (tail x))) :- $(parseInts xs)  |] 
  | otherwise = [| ($(varE (mkName x))) :- $(parseInts xs) |] 

