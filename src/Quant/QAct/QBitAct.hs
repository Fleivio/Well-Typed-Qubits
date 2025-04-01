module QAct.QBitAct(
  QBitAct
  , module QAct.QAct
  , h
  , x
  , y
  , z
  , cnot
  , entangle
  , cz
  , fredkin
  , swap
  , p
  , t
  , s
  , toffoli
  , measureBool
  , measureNBool
  , parallel
  , toState
  , oracle) where

import QAct.QAct
import Unsafe.Coerce
import Control.Monad
import Control.Monad.Reader

type QBitAct s a = QAct Bit s a

measureBool :: (KnownNat ix, ValidSelector '[ix] n) => SNat ix -> QBitAct n Bool
measureBool k = do
  a <-  measure k
  return $ unsafeCoerce a

measureNBool :: ValidSelector acs n => SList acs -> QBitAct n (NList Bool (Length acs))
measureNBool ks = do
  as <- measureN ks
  return $ unsafeCoerce as

parallel :: KnownNat n => QBitAct n a -> QBitAct n a
parallel act = do
  appAll_ h
  val <- act
  appAll_ h
  return val

toState :: Bit -> QBitAct 1 ()
toState a = do
  k <- measure (SNat @1)
  when (k /= a) $ app [qb|1|] x

h :: QBitAct 1 ()
h = qActMatrix [
      ((0:>NNil, 0:>NNil), recip $ sqrt 2),
      ((0:>NNil, 1:>NNil), recip $ sqrt 2),
      ((1:>NNil, 0:>NNil), recip $ sqrt 2),
      ((1:>NNil, 1:>NNil), -(recip $ sqrt 2))
    ]

x :: QBitAct 1 ()
x = qActMatrix [
      ((0:>NNil, 1:>NNil), 1),
      ((1:>NNil, 0:>NNil), 1)
    ]

y :: QBitAct 1 ()
y = qActMatrix [
      ((0:>NNil, 1:>NNil), -1),
      ((1:>NNil, 0:>NNil), 1)
    ]


p :: Double -> QBitAct 1 ()
p l = qActMatrix [
      ((0:>NNil, 0:>NNil), 1),
      ((1:>NNil, 1:>NNil), exp (0 :+ l))
    ]

z :: QBitAct 1 ()
z = qActMatrix [
      ((0:>NNil, 0:>NNil), 1),
      ((1:>NNil, 1:>NNil), -1)
    ]

s :: QBitAct 1 ()
s = qActMatrix [
      ((0:>NNil, 0:>NNil), 1),
      ((1:>NNil, 1:>NNil), 0 :+ 1)
    ]

t :: QBitAct 1 ()
t = qActMatrix [
      ((0:>NNil, 0:>NNil), 1),
      ((1:>NNil, 1:>NNil), (1 :+ 1)/sqrt 2)
    ]

cnot :: QBitAct 2 ()
cnot = qActMatrix [
      ((0:>0:>NNil, 0:>0:>NNil), 1),
      ((0:>1:>NNil, 0:>1:>NNil), 1),
      ((1:>0:>NNil, 1:>1:>NNil), 1),
      ((1:>1:>NNil, 1:>0:>NNil), 1)
    ]

entangle :: QBitAct 2 ()
entangle = do
  app (SNat @1 :- SNil) h
  app (SNat @1 :- SNat @2 :- SNil) cnot

toffoli :: QBitAct 3 ()
toffoli = qActMatrix [
      ((0:>0:>0:>NNil, 0:>0:>0:>NNil), 1),
      ((0:>0:>1:>NNil, 0:>0:>1:>NNil), 1),
      ((0:>1:>0:>NNil, 0:>1:>0:>NNil), 1),
      ((0:>1:>1:>NNil, 0:>1:>1:>NNil), 1),
      ((1:>0:>0:>NNil, 1:>0:>0:>NNil), 1),
      ((1:>0:>1:>NNil, 1:>0:>1:>NNil), 1),
      ((1:>1:>0:>NNil, 1:>1:>1:>NNil), 1),
      ((1:>1:>1:>NNil, 1:>1:>0:>NNil), 1)
    ]

cz :: QBitAct 2 ()
cz = qActMatrix [
      ((0:>0:>NNil, 0:>0:>NNil), 1),
      ((0:>1:>NNil, 0:>1:>NNil), 1),
      ((1:>0:>NNil, 1:>0:>NNil), 1),
      ((1:>1:>NNil, 1:>1:>NNil), -1)
    ]

fredkin :: QBitAct 3 ()
fredkin = qActMatrix [
      ((0:>0:>0:>NNil, 0:>0:>0:>NNil), 1),
      ((0:>0:>1:>NNil, 0:>0:>1:>NNil), 1),
      ((0:>1:>0:>NNil, 0:>1:>0:>NNil), 1),
      ((0:>1:>1:>NNil, 0:>1:>1:>NNil), 1),
      ((1:>0:>0:>NNil, 1:>0:>0:>NNil), 1),
      ((1:>0:>1:>NNil, 1:>1:>0:>NNil), 1),
      ((1:>1:>0:>NNil, 1:>0:>1:>NNil), 1),
      ((1:>1:>1:>NNil, 1:>1:>1:>NNil), 1)
    ]

swap :: QBitAct 2 ()
swap = qActMatrix [
      ((0:>0:>NNil, 0:>0:>NNil), 1),
      ((0:>1:>NNil, 1:>0:>NNil), 1),
      ((1:>0:>NNil, 0:>1:>NNil), 1),
      ((1:>1:>NNil, 1:>1:>NNil), 1)
    ]

oracle :: forall ctrs trgs
  . ValidSelector (ctrs <++> trgs) (Length ctrs + Length trgs)
  => (NList Bit (Length ctrs) -> Bool) 
  -> SList ctrs
  -> SList trgs
  -> QBitAct (Length ctrs + Length trgs) ()
oracle f control taget = do
  let
    ctrCount = length $ sListToList control
    trgCount = length $ sListToList taget
    change = [((ctr ++ trg, ctr ++ flipTrg), 1 :: PA) |
            ctr <- basis @Bit ctrCount,
            f $ unsafeCoerce ctr,
            trg <- basis @Bit trgCount,
            let flipTrg = negate <$> trg
            ]
    unchange = [((ctr ++ trg, ctr ++ trg), 1) |
            ctr <- basis @Bit ctrCount,
            not $ f $ unsafeCoerce ctr,
            trg <- basis @Bit trgCount
            ]
    op = mkOP (change ++ unchange)
  
  qv <- ask
  let qv' = unsafeSelectQ (control `sListConcat` taget) qv
  liftIO $ appV op qv'