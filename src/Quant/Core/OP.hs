module Core.OP
  ( mkOP
  , appOP
  , getOpProb
  , OP(..)
  , module Core.QV
  , _h
  ) where

import           Data.List
import           Data.Map    as Map
import           Core.QV

data OP a = OP {
    opSize :: Int,
    opMap :: Map ([a], [a]) PA
  }

instance Show a => Show (OP a) where
  show (OP _ qop) =
    intercalate "\n" $ do
      ((a, b), pa) <- toList qop
      return $ show pa ++ "|" ++ show a ++ "⟩⟨" ++ show b ++ "|"

getOpProb :: Ord a => OP a -> ([a], [a]) -> PA
getOpProb (OP _ qmap) index = Map.findWithDefault 0 index qmap

mkOP :: Ord a => [(([a], [a]), PA)] -> OP a
mkOP l = OP (length $ fst $ head l) (fromList l)

appOP ::
     (Ord a, Basis a)
  => OP a
  -> QV a
  -> QV a
appOP qop qv = mkQV [(b, prob b) | b <- basis (opSize qop)]
  where
    prob b = sum [qop `getOpProb` (a, b) * qv `getProb` a | a <- basis (opSize qop)]

_h :: OP Bit
_h = mkOP [
      (([O], [O]), 1 :+ 0),
      (([O], [I]), 1 :+ 0),
      (([I], [O]), 1 :+ 0),
      (([I], [I]), (-1) :+ 0)
    ]