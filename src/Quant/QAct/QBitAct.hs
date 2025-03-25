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

parallel :: ValidSelector acs n => SList acs -> QBitAct n a -> QBitAct n a
parallel sl act = do
  _ <- mapp sl h
  val <- act
  _ <- mapp sl h
  return val

toState :: Bit -> QBitAct 1 ()
toState a = do
  k <- measure (SNat @1)
  when (k /= a) $ app [qb|1|] x

h :: QBitAct 1 ()
h = qActMatrix [
      ((O:>NNil, O:>NNil), recip $ sqrt 2),
      ((O:>NNil, I:>NNil), recip $ sqrt 2),
      ((I:>NNil, O:>NNil), recip $ sqrt 2),
      ((I:>NNil, I:>NNil), -(recip $ sqrt 2))
    ]

x :: QBitAct 1 ()
x = qActMatrix [
      ((O:>NNil, I:>NNil), 1),
      ((I:>NNil, O:>NNil), 1)
    ]

y :: QBitAct 1 ()
y = qActMatrix [
      ((O:>NNil, I:>NNil), -1),
      ((I:>NNil, O:>NNil), 1)
    ]


p :: Double -> QBitAct 1 ()
p l = qActMatrix [
      ((O:>NNil, O:>NNil), 1),
      ((I:>NNil, I:>NNil), exp (0 :+ l))
    ]

z :: QBitAct 1 ()
z = qActMatrix [
      ((O:>NNil, O:>NNil), 1),
      ((I:>NNil, I:>NNil), -1)
    ]

s :: QBitAct 1 ()
s = qActMatrix [
      ((O:>NNil, O:>NNil), 1),
      ((I:>NNil, I:>NNil), 0 :+ 1)
    ]

t :: QBitAct 1 ()
t = qActMatrix [
      ((O:>NNil, O:>NNil), 1),
      ((I:>NNil, I:>NNil), (1 :+ 1)/sqrt 2)
    ]

cnot :: QBitAct 2 ()
cnot = qActMatrix [
      ((O:>O:>NNil, O:>O:>NNil), 1),
      ((O:>I:>NNil, O:>I:>NNil), 1),
      ((I:>O:>NNil, I:>I:>NNil), 1),
      ((I:>I:>NNil, I:>O:>NNil), 1)
    ]

entangle :: QBitAct 2 ()
entangle = do
  app (SNat @1 :- SNil) h
  app (SNat @1 :- SNat @2 :- SNil) cnot

toffoli :: QBitAct 3 ()
toffoli = qActMatrix [
      ((O:>O:>O:>NNil, O:>O:>O:>NNil), 1),
      ((O:>O:>I:>NNil, O:>O:>I:>NNil), 1),
      ((O:>I:>O:>NNil, O:>I:>O:>NNil), 1),
      ((O:>I:>I:>NNil, O:>I:>I:>NNil), 1),
      ((I:>O:>O:>NNil, I:>O:>O:>NNil), 1),
      ((I:>O:>I:>NNil, I:>O:>I:>NNil), 1),
      ((I:>I:>O:>NNil, I:>I:>I:>NNil), 1),
      ((I:>I:>I:>NNil, I:>I:>O:>NNil), 1)
    ]

cz :: QBitAct 2 ()
cz = qActMatrix [
      ((O:>O:>NNil, O:>O:>NNil), 1),
      ((O:>I:>NNil, O:>I:>NNil), 1),
      ((I:>O:>NNil, I:>O:>NNil), 1),
      ((I:>I:>NNil, I:>I:>NNil), -1)
    ]

fredkin :: QBitAct 3 ()
fredkin = qActMatrix [
      ((O:>O:>O:>NNil, O:>O:>O:>NNil), 1),
      ((O:>O:>I:>NNil, O:>O:>I:>NNil), 1),
      ((O:>I:>O:>NNil, O:>I:>O:>NNil), 1),
      ((O:>I:>I:>NNil, O:>I:>I:>NNil), 1),
      ((I:>O:>O:>NNil, I:>O:>O:>NNil), 1),
      ((I:>O:>I:>NNil, I:>I:>O:>NNil), 1),
      ((I:>I:>O:>NNil, I:>O:>I:>NNil), 1),
      ((I:>I:>I:>NNil, I:>I:>I:>NNil), 1)
    ]

swap :: QBitAct 2 ()
swap = qActMatrix [
      ((O:>O:>NNil, O:>O:>NNil), 1),
      ((O:>I:>NNil, I:>O:>NNil), 1),
      ((I:>O:>NNil, O:>I:>NNil), 1),
      ((I:>I:>NNil, I:>I:>NNil), 1)
    ]

oracle :: forall controls targets
  .(ValidSelector (controls <++> targets) (Length controls + Length targets)) 
  => (NList Bit (Length controls) -> Bool) 
  -> SList controls
  -> SList targets
  -> QBitAct (Length controls + Length targets) ()
oracle f contrs targs = do
  let
    controlCount = length $ sListToList contrs
    targetCount = length $ sListToList targs
    change = [((control ++ target, control ++ flippedTargs), 1 :: PA) |
            control <- basis @Bit controlCount,
            f $ unsafeCoerce control,
            target <- basis @Bit targetCount,
            let flippedTargs = map (\case {O -> I; I -> O}) target
            ]
    unchange = [((control ++ target, control ++ target), 1) |
            control <- basis @Bit controlCount,
            not $ f $ unsafeCoerce control,
            target <- basis @Bit targetCount
            ]
    op = mkOP (change ++ unchange)
  
  qv <- ask
  let qv' = unsafeSelectQ (contrs `sListConcat` targs) qv
  liftIO $ appV op qv'