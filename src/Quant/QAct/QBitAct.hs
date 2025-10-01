{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
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
  , swap
  , p
  , t
  , s
  , toffoli
  , measureBool
  , measureNBool
  , parallel
  , toState
  , oracle
  , rx
  , ry
  , rz) where

import QAct.QAct
import Unsafe.Coerce
import Control.Monad
import Control.Monad.Reader
import Quoters.BitQuoter
import Quoters.SListQuoter
import Quoters.MatrixQuoter

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
qid = [matrix|
  0 =[1]=> 0
  1 =[1]=> 1
|]

h :: QBitAct 1 ()
h = [matrix|
    0 =[1/sqrt 2 ]=> 0
    0 =[1/sqrt 2 ]=> 1
    1 =[1/sqrt 2 ]=> 0
    1 =[-1/sqrt 2]=> 1
|]

x :: QBitAct 1 ()
x = [matrix|
  0 =[1]=> 1
  1 =[1]=> 0
|] 

y :: QBitAct 1 ()
y = [matrix|
  0 =[-1]=> 1
  1 =[1 ]=> 0
|]

p :: Double -> QBitAct 1 ()
p angle = [matrix|
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
cnot = [matrix|
  0 0 =[1]=> 0 0
  0 1 =[1]=> 0 1
  1 0 =[1]=> 1 1
  1 1 =[1]=> 1 0 
|]

entangle :: QBitAct 2 ()
entangle = do
  app [qb|1|] h
  app [qb|1 2|] cnot

toffoli :: QBitAct 3 ()
toffoli = [matrix|
  0 0 0 =[1]=> 0 0 0
  0 0 1 =[1]=> 0 0 1
  1 0 0 =[1]=> 1 0 0
  1 0 1 =[1]=> 1 0 1
  0 1 0 =[1]=> 0 1 0
  0 1 1 =[1]=> 0 1 1
  1 1 0 =[1]=> 1 1 1
  1 1 1 =[1]=> 1 1 0
|]

cz :: QBitAct 2 ()
cz = [matrix|
  0 0 =[1]=> 0 0
  0 1 =[1]=> 0 1
  1 0 =[1]=> 1 0
  1 1 =[-1]=> 1 1
|]  

fredkin :: QBitAct 3 ()
fredkin = [matrix|
  0 0 0 =[1]=> 0 0 0
  0 0 1 =[1]=> 0 0 1
  0 1 0 =[1]=> 0 1 0
  0 1 1 =[1]=> 0 1 1
  1 0 0 =[1]=> 1 0 0
  1 0 1 =[1]=> 1 1 0
  1 1 0 =[1]=> 1 0 1
  1 1 1 =[1]=> 1 1 1
  |]

swap :: QBitAct 2 ()
swap = [matrix|
  0 0 =[1]=> 0 0
  0 1 =[1]=> 1 0
  1 0 =[1]=> 0 1
  1 1 =[1]=> 1 1
|]

rx :: Double -> QBitAct 1 ()
rx angle = [matrix|
  0 =[cos(angle/2) :+ 0]=> 0
  0 =[0 :+ -sin (angle/2)]=> 1
  1 =[0 :+ -sin (angle/2)]=> 0
  1 =[cos(angle/2) :+ 0]=> 1
|]

ry :: Double -> QBitAct 1 ()
ry angle = [matrix|
  0 =[cos(angle/2) :+ 0]=> 0
  0 =[sin (angle/2) :+ 0]=> 1
  1 =[-sin (angle/2) :+ 0]=> 0
  1 =[cos(angle/2) :+ 0]=> 1
|]

rz :: Double -> QBitAct 1 ()
rz angle = [matrix|
  0 =[exp(0 :+ -angle/2)]=> 0
  1 =[exp(0 :+ angle/2)]=> 1
|]

oracle :: forall ctrs trgs
  . ValidSelector (ctrs <++> trgs) (Length ctrs + Length trgs)
  => (Vec (Length ctrs) Bit -> Bool)  --control function
  -> SList ctrs                       --controls
  -> SList trgs                       --targets
  -> QBitAct (Length ctrs + Length trgs) ()
oracle enable control taget = do
  let
    ctrCount = length $ sListToList control
    trgCount = length $ sListToList taget
    change = [((ctr ++ trg, ctr ++ flipTrg), 1) |
            ctr <- basis @Bit ctrCount,
            enable $ unsafeCoerce ctr,
            trg <- basis @Bit trgCount,
            let flipTrg = negate <$> trg --cnot
            ]
    unchange = [((ctr ++ trg, ctr ++ trg), 1) |
            ctr <- basis @Bit ctrCount,
            not $ enable $ unsafeCoerce ctr,
            trg <- basis @Bit trgCount
            ]
    op = mkOP (change ++ unchange)
  
  qv <- ask
  let qv' = unsafeSelectQ (control `sListConcat` taget) qv
  liftIO $ appV op qv'