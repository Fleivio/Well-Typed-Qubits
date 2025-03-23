module BitQuoter(nl, mkq) where

import List.NList
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
    parseBit "I" = [| I |]
    parseBit "O" = [| O |]
    parseBit _   = error "Invalid Bit value: must be 'I' or 'O'"

parseNListPat :: String -> Q Pat
parseNListPat input = do
  let vars = words input
  foldr (\var acc -> [p| $(varP (mkName var)) :> $acc |]) [p| NNil |] vars

---------------------------------
-- [mkq|O O I|] = mkQ [(nl, prob1), (nl, prob2)]

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
    