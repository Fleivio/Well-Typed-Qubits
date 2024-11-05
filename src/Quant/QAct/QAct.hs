module QAct.QAct
  ( QAct
  , Matrix
  , runQ
  , qActMatrix
  , app
  , sample
  , measure
  , liftIO
  , module Core.Virt
  , module List.NList
  , module List.SList
  -- , getCurrentIndexes
  ) where

import Core.Virt
import List.NList
import List.SList

import Control.Monad.Reader
import Data.Kind
import Unsafe.Coerce

type QAct :: Type -> Natural -> Type -> Type
type QAct b s a = ReaderT (Virt b s) IO a

type Matrix b s = [((NList b s, NList b s), PA)]

runQ :: QAct b s a -> Virt b s -> IO a
runQ = runReaderT

qActMatrix :: Basis b => Matrix b s -> QAct b s ()
qActMatrix mat = do
  let op = mkOP $ unsafeCoerce mat
  vv <- ask
  lift $ appV op vv

app ::
     ValidSelector acs n
  => SList acs
  -> QAct b (Length acs) a
  -> QAct b n a
app sl act = do
  qv <- ask
  let adaptedValue = selectQ sl qv
  lift $ runReaderT act adaptedValue

mapp :: Show b => ValidSelector acs n
  => SList acs
  -> QAct b 1 ()
  -> QAct b n ()
mapp acs act = go acs act
  where 
    go :: Show b => SList acs -> QAct b 1 () -> QAct b n ()
    go SNil act = return ()
    go (x :- xs) act = do
      sample
      qv <- ask
      let adaptedValue = selectQ (x :- SNil) qv
      lift $ runReaderT act adaptedValue
      lift $ runReaderT (go xs act) qv


sample :: Show b => QAct b s () 
sample = do
  virt <- ask
  lift $ printQ virt

measure :: (KnownNat ix, Basis b, ValidSelector '[ix] n) => SNat ix -> QAct b n b
measure sn = do
  virt <- ask
  lift $ measureV virt (fromIntegral $ natVal sn)
