module QAct.QAct
  ( QAct
  , Matrix
  , runQ
  , runQHist
  , qActMatrix
  , app
  , sample
  , measure
  , liftIO
  , getCurrentIndexes
  , module Core.Virt
  , module List.NList
  , module List.SList
  ) where

import Core.Virt
import List.NList
import List.SList
import QAct.QHistory

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad
import Data.Kind
import Unsafe.Coerce

type QAct :: Type -> Natural -> Type -> Type
type QAct b s a = WriterT QHistory (ReaderT (Virt b s) IO) a

type Matrix b s = [((NList b s, NList b s), PA)]

runQ :: QAct b s a -> Virt b s -> IO a
runQ qa val = fst <$> runQHist qa val 

runQHist :: QAct b s a -> Virt b s -> IO (a, QHistory)
runQHist qa = runReaderT (runWriterT qa)

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
  let adaptedValue = selectQ sl qv
  (a, h) <- liftIO $ runQHist act adaptedValue
  tell h
  return a

sample :: Show b => QAct b s () 
sample = do
  virt <- ask
  str <- liftIO $ showQ virt
  liftIO $ putStrLn str
  tell [Sample str]

measure :: (KnownNat ix, Basis b, ValidSelector '[ix] n) => SNat ix -> QAct b n b
measure sn = do
  virt <- ask
  tell [Measure $ fromIntegral $ natVal sn]
  liftIO $ measureV virt (fromIntegral $ natVal sn)

getCurrentIndexes :: QAct b s [Int]
getCurrentIndexes = do
  Virt _ acs <- ask
  return $ unsafeCoerce acs
