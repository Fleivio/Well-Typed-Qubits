module Core.QV
  ( QV
  , getProb
  , mkQV
  , norm
  , normalize
  , module Core.PA
  , module Core.Basis
  ) where

import           Core.Basis
import           Core.PA
import           Data.List
import           Data.Map    as Map

data QV a = QV
  {
    qvSize :: Int,
    qvMap :: Map [a] PA
  } 

getProb :: Basis a => QV a -> [a] -> PA
getProb qv index = Map.findWithDefault 0 index (qvMap qv)

instance Basis a => Semigroup (QV a) where
  qv1 <> qv2 = 
    mkQV [ (x ++ y, getProb qv1 x * getProb qv2 y)
        | x <- basis $ qvSize qv1
        , y <- basis $ qvSize qv2
        ]

instance Show a => Show (QV a) where 
  show qv =
    intercalate " + " $ do
      (a, pa) <- toList $ qvMap qv
      return
        $ case pa of
            0 -> mempty
            _ -> showPAMultiplicative pa ++ show a


mkQV :: Basis a => [([a], PA)] -> QV a
mkQV vals = normalize $ QV (length (fst $ head vals)) (k vals) 
  where k = Map.fromList . Prelude.filter ((/= 0) . snd)

norm :: QV a -> Double
norm v = sqrt . sum $ probs
  where
    probs = squareModulus . snd <$> toList (qvMap v)

normalize :: QV a -> QV a
normalize (QV l qval) = QV l $ (c *) `Map.map` qval
  where
    c = 1 / norm (QV l qval) :+ 0
