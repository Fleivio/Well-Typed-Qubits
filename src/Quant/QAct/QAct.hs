module QAct.QAct
  ( QAct
  , Matrix
  , runQ
  , qActMatrix
  , app
  , sample
  , measure
  , liftIO
  , measureN
  , phaseOracle
  , mapp
  , mapp_
  , appAll
  , appAll_
  , module List.Vec
  , module List.SList
  , module Core.Virt
  ) where

import Core.Virt
import List.Vec
import List.SList

import Control.Monad.Reader
import Data.Kind
import Unsafe.Coerce
import Data.Data (Proxy(..))

type QAct :: Type -> Natural -> Type -> Type
type QAct b s a = ReaderT (Virt b s) IO a

type Matrix s b = [((Vec s b, Vec s b), PA)]

runQ :: QAct b s a -> Virt b s -> IO a
runQ = runReaderT

qActMatrix :: Basis b => Matrix s b -> QAct b s ()
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
  liftIO $ measureVirt virt (fromIntegral $ natVal sn)

measureN :: (Basis b, ValidSelector acs n) => SList acs -> QAct b n (Vec (Length acs) b)
measureN ks = do
  qv <- ask
  let list = sListToList ks
  result <- liftIO $ measureVirtN qv list
  return $ unsafeCoerce result

mapp :: ValidSelector acs n => SList acs -> QAct b 1 a -> QAct b n [a]
mapp sl op = do
  qv <- ask
  let list = sListToList sl
  liftIO $ sequence [runQ op $ unsafeCoerce (unsafeSelectQ (unsafeCoerce [ix]) qv) | ix <- list]

mapp_ :: ValidSelector acs n => SList acs -> QAct b 1 a -> QAct b n ()
mapp_ sl op = do 
  _ <- mapp sl op
  return ()

phaseOracle :: forall b n. (Basis b, Show b, KnownNat n) => (Vec n b -> Bool) -> QAct b n ()
phaseOracle f = do
  let
      op = mkOP @b
          [ ((b,b), if f $ unsafeCoerce b then -1 else 1)
           | b <- basis @b $ fromIntegral (natVal (Proxy @n))]
  vv <- ask
  liftIO $ appV op vv

appAll :: forall b n a. KnownNat n => QAct b 1 a -> QAct b n [a]
appAll op = do
  qv <- ask
  liftIO $ sequence
    [runQ op $ unsafeCoerce (unsafeSelectQ (unsafeCoerce [ix]) qv)
      | ix <- [(1 :: Int)..fromIntegral $ natVal $ Proxy @n]]

appAll_ :: forall b n a. KnownNat n => QAct b 1 a -> QAct b n ()
appAll_ op = do
  _ <- appAll op
  return ()