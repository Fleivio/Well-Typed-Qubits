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
  , toffoli) where

import QAct.QAct

type QBitAct s a = QAct Bit s a

h :: QBitAct 1 ()
h = qActMatrix [
      ((O:>NNil, O:>NNil), 1 :+ 0),
      ((O:>NNil, I:>NNil), 1 :+ 0),
      ((I:>NNil, O:>NNil), 1 :+ 0),
      ((I:>NNil, I:>NNil), (-1) :+ 0)
    ]

x :: QBitAct 1 ()
x = qActMatrix [
      ((O:>NNil, I:>NNil), 1 :+ 0),
      ((I:>NNil, O:>NNil), 1 :+ 0)
    ]

y :: QBitAct 1 ()
y = qActMatrix [
      ((O:>NNil, I:>NNil), (-1):+ 0),
      ((I:>NNil, O:>NNil), 1:+ 0)
    ]


p :: Double -> QBitAct 1 ()
p l = qActMatrix [
      ((O:>NNil, O:>NNil), 1:+ 0),
      ((I:>NNil, I:>NNil), exp (0 :+ (pi*l)))
    ]

z :: QBitAct 1 ()
z = p pi

s :: QBitAct 1 ()
s = p (pi/2)

t :: QBitAct 1 ()
t = p (pi/4)

cnot :: QBitAct 2 ()
cnot = qActMatrix [
      ((O:>O:>NNil, O:>O:>NNil), 1 :+ 0),
      ((O:>I:>NNil, O:>I:>NNil), 1 :+ 0),
      ((I:>O:>NNil, I:>I:>NNil), 1 :+ 0),
      ((I:>I:>NNil, I:>O:>NNil), 1 :+ 0)
    ]

entangle :: QBitAct 2 ()
entangle = do
  app (SNat @1 :- SNil) h
  app (SNat @1 :- SNat @2 :- SNil) cnot

toffoli :: QBitAct 3 ()
toffoli = qActMatrix [
      ((O:>O:>O:>NNil, O:>O:>O:>NNil), 1:+ 0),
      ((O:>O:>I:>NNil, O:>O:>I:>NNil), 1:+ 0),
      ((O:>I:>O:>NNil, O:>I:>O:>NNil), 1:+ 0),
      ((O:>I:>I:>NNil, O:>I:>I:>NNil), 1:+ 0),
      ((I:>O:>O:>NNil, I:>O:>O:>NNil), 1:+ 0),
      ((I:>O:>I:>NNil, I:>O:>I:>NNil), 1:+ 0),
      ((I:>I:>O:>NNil, I:>I:>I:>NNil), 1:+ 0),
      ((I:>I:>I:>NNil, I:>I:>O:>NNil), 1:+ 0)
    ]

cz :: QBitAct 2 ()
cz = qActMatrix [
      ((O:>O:>NNil, O:>O:>NNil), 1:+ 0),
      ((O:>I:>NNil, O:>I:>NNil), 1:+ 0),
      ((I:>O:>NNil, I:>O:>NNil), 1:+ 0),
      ((I:>I:>NNil, I:>I:>NNil), (-1):+ 0)
    ]

fredkin :: QBitAct 3 ()
fredkin = qActMatrix [
      ((O:>O:>O:>NNil, O:>O:>O:>NNil), 1:+ 0),
      ((O:>O:>I:>NNil, O:>O:>I:>NNil), 1:+ 0),
      ((O:>I:>O:>NNil, O:>I:>O:>NNil), 1:+ 0),
      ((O:>I:>I:>NNil, O:>I:>I:>NNil), 1:+ 0),
      ((I:>O:>O:>NNil, I:>O:>O:>NNil), 1:+ 0),
      ((I:>O:>I:>NNil, I:>I:>O:>NNil), 1:+ 0),
      ((I:>I:>O:>NNil, I:>O:>I:>NNil), 1:+ 0),
      ((I:>I:>I:>NNil, I:>I:>I:>NNil), 1:+ 0)
    ]

swap :: QBitAct 2 ()
swap = qActMatrix [
      ((O:>O:>NNil, O:>O:>NNil), 1:+ 0),
      ((O:>I:>NNil, I:>O:>NNil), 1:+ 0),
      ((I:>O:>NNil, O:>I:>NNil), 1:+ 0),
      ((I:>I:>NNil, I:>I:>NNil), 1:+ 0)
    ]