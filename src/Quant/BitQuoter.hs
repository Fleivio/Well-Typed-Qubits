module BitQuoter(nl, mkq) where

import List.Vec
import Core.Virt
import Text.Read (readMaybe)


import Language.Haskell.TH hiding (Type)
import Language.Haskell.TH.Quote


nl :: QuasiQuoter
nl = QuasiQuoter
  { quoteExp  = parseNList
  , quotePat  = parseNListPat
  , quoteType = undefined
  , quoteDec  = undefined
  }

parseNList :: String -> Q Exp
parseNList input = do
  let bits = map parseBit (words input)
  foldr (\bit acc -> [| $bit :> $acc |]) [| NNil |] bits
  where
    parseBit "1" = [| 1 |]
    parseBit "I" = [| 1 |]
    parseBit "0" = [| 0 |]
    parseBit "O" = [| 0 |]
    parseBit _   = error "Invalid Bit value: must be '1' or '0'"

parseNListPat :: String -> Q Pat
parseNListPat input = do
  let vars = words input
  foldr (\var acc ->  [p| $(varP (mkName var)) :> $acc |]) [p| NNil |] vars

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
    