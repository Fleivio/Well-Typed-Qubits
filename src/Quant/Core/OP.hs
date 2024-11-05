{-# OPTIONS_GHC -Wno-x-partial #-}

module Core.OP
  ( mkOP
  , appOP
  , getOpProb
  , OP(..)
  , _h
  , module Core.QV
  ) where

import Core.QV

import Data.List
import Data.Map as Map

data OP a = OP {
    opSize :: Int,
    opMap :: Map ([a], [a]) PA
  }

instance Show a => Show (OP a) where
  show (OP _ qop) =
    intercalate "\n" $ do
      ((a, b), pa) <- toList qop
      return $ show pa ++ showKet a ++ showBra b

getOpProb :: Ord a => OP a -> ([a], [a]) -> PA
getOpProb (OP _ qmap) index = Map.findWithDefault 0 index qmap

mkOP :: Ord a => [(([a], [a]), PA)] -> OP a
mkOP l = OP (length $ fst $ fst $ head l) (fromList l)

appOP ::
     (Ord a, Basis a)
  => OP a
  -> QV a
  -> QV a
appOP qop qv =
  if opSize qop /= qvSize qv
    then error "Dimension mismatch"
    else mkQV [(b, prob b) | b <- basis (opSize qop)]
  where
    prob b = sum [qop `getOpProb` (a, b) * qv `getProb` a | a <- basis (opSize qop)]

instance Ord a => Semigroup (OP a) where
  OP _ m1 <> OP _ m2 = mkOP 
    [((a1 ++ b1, a2 ++ b2), pa1 * pa2) 
    | ((a1, a2), pa1) <- toList m1, ((b1, b2), pa2) <- toList m2]

_h :: OP Bit
_h = mkOP [(([O],[O]),1),(([O],[I]),1),(([I],[O]),1),(([I],[I]),-1)]