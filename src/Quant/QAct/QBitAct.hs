{-# OPTIONS_GHC -Wno-name-shadowing -Wno-incomplete-uni-patterns #-}
module QAct.QBitAct(
  QBitAct
  , module QAct.QAct
  , qid
  , h
  , x
  , y
  , z
  , cnot
  , entangle
  , cz
  , fredkin
  , fredkinn
  , swap
  , p
  , t
  , s
  , toffoli
  , measureBool
  , measureNBool
  , parallel
  -- , controlledAct
  -- , controlled
  , toState
  , rx
  , ry
  , rz
  , cnotn) where

import QAct.QAct
import Unsafe.Coerce
import Control.Monad
import Control.Monad.Reader
import Quoters.SListQuoter
import Quoters.MatrixQuoter
import Data.Proxy (Proxy(..))
import Quoters.BitQuoter (vec)

type QBitAct s a = QAct Bit s a

measureBool :: (KnownNat ix, ValidSelector '[ix] n) => SNat ix -> QBitAct n Bool
measureBool k = do
  a <-  measure k
  return $ unsafeCoerce a

measureNBool :: ValidSelector acs n => SList acs -> QBitAct n (Vec (Length acs) Bool)
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

qid :: QBitAct 1 ()
qid = qActMatrix [matrix|
  0 =[1]=> 0
  1 =[1]=> 1
|]

h :: QBitAct 1 ()
h = qActMatrix [matrix|
    0 =[1/sqrt 2 ]=> 0
    0 =[1/sqrt 2 ]=> 1
    1 =[1/sqrt 2 ]=> 0
    1 =[-1/sqrt 2]=> 1
|]

x :: QBitAct 1 ()
x = qActMatrix [matrix|
  0 =[1]=> 1
  1 =[1]=> 0
|]

y :: QBitAct 1 ()
y = qActMatrix [matrix|
  0 =[-1]=> 1
  1 =[1 ]=> 0
|]

p :: Double -> QBitAct 1 ()
p angle = qActMatrix [matrix|
  0 =[1]=> 0
  1 =[exp (0 :+ angle)]=> 1 
|]


z :: QBitAct 1 ()
z = p pi

s :: QBitAct 1 ()
s = p (pi/2)

t :: QBitAct 1 ()
t = p (pi/8)

cnot :: QBitAct 2 ()
cnot = control (== [vec|1|]) (fmap negate)

toffoli :: QBitAct 3 ()
toffoli = control (== [vec|1 1|]) (fmap negate) 

cnotn :: forall k l n. Partition k l n => QBitAct n ()
cnotn = control (== [vec|k*1|]) (fmap negate)

entangle :: QBitAct 2 ()
entangle = do
  app [qb|1|] h
  app [qb|1 2|] cnot

cz :: QBitAct 2 ()
cz = qActMatrix [matrix|
  0 0 =[1]=> 0 0
  0 1 =[1]=> 0 1
  1 0 =[1]=> 1 0
  1 1 =[-1]=> 1 1
|]

swap :: QBitAct 2 ()
swap = liftQ (\[vec|x y|] -> [vec|y x|])

fredkin :: QBitAct 3 ()
fredkin = control (== [vec|1|]) (\[vec|x y|] -> [vec|y x|])

fredkinn :: forall n k. Partition n 2 k => QBitAct k ()
fredkinn = control (== [vec|n*1|]) ((\[vec|x y|] -> [vec|y x|]))

rx :: Double -> QBitAct 1 ()
rx angle = qActMatrix [matrix|
  0 =[cos(angle/2) :+ 0]=> 0
  0 =[0 :+ -sin (angle/2)]=> 1
  1 =[0 :+ -sin (angle/2)]=> 0
  1 =[cos(angle/2) :+ 0]=> 1
|]

ry :: Double -> QBitAct 1 ()
ry angle = qActMatrix [matrix|
  0 =[cos(angle/2) :+ 0]=> 0
  0 =[sin (angle/2) :+ 0]=> 1
  1 =[-sin (angle/2) :+ 0]=> 0
  1 =[cos(angle/2) :+ 0]=> 1
|]

rz :: Double -> QBitAct 1 ()
rz angle = qActMatrix [matrix|
  0 =[exp(0 :+ -angle/2)]=> 0
  1 =[exp(0 :+ angle/2)]=> 1
|]