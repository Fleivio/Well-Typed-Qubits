module QAct.QAct
  ( QAct
  , Matrix
  , runQ
  , qActMatrix
  , app
  , sample
  , measure
  , liftIO
  , (<#>)
  , module Core.Virt
  , module List.NList
  , module List.SList
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
  liftIO $ appV op vv

app ::
     ValidSelector acs n
  => SList acs
  -> QAct b (Length acs) a
  -> QAct b n a
app sl act = do
  qv <- ask
  let adaptedValue = unsafeSelectQ sl qv
  liftIO $ runReaderT act adaptedValue

sample :: Show b => QAct b s () 
sample = do
  virt <- ask
  liftIO $ printQ virt

measure :: (KnownNat ix, Basis b, ValidSelector '[ix] n) => SNat ix -> QAct b n b
measure sn = do
  virt <- ask
  liftIO $ measureV virt (fromIntegral $ natVal sn)

(<#>) :: QAct b s a1 -> QAct b s a2 -> QAct b s a2
op1 <#> op2 = do
  qv <- ask
  _ <- liftIO $ runReaderT op1 qv
  liftIO $ runReaderT op2 qv