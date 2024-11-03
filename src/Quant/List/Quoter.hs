module List.Quoter(qb) where 

import Language.Haskell.TH
import Language.Haskell.TH.Quote

qb :: QuasiQuoter
qb = QuasiQuoter
  { quoteExp  = slistExp
  , quotePat  = undefined
  , quoteType = undefined
  , quoteDec  = undefined
  }

slistExp :: String -> Q Exp
slistExp str = do
  let nums = words str
  buildSList nums

buildSList :: [String] -> Q Exp
buildSList []     = [| SNil |]
buildSList (x:xs) = do
  let n = read x :: Integer
  [| SNat @($(litT (numTyLit n))) :- $(buildSList xs) |]
