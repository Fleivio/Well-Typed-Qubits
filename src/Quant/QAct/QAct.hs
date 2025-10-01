{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use void" #-}
module QAct.QAct
  ( QAct
  , Matrix
  , runQ
  , liftIO
  , qActMatrix
  , app
  , app_
  , appAll
  , appAll_
  , mapp
  , mapp_
  , sample
  , measure
  , measureN
  , measureAll
  , (<-@>)
  , (<@>)
  , phaseOracle
  , module List.Vec
  , module List.SList
  , module Core.Virt
  , Partition
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

--[APP]--------------------------------------------------------

app ::
     ValidSelector acs n
  => SList acs
  -> QAct b (Length acs) a
  -> QAct b n a
app sl act = do
  qv <- ask
  let adaptedValue = unsafeSelectQ sl qv
  liftIO $ runReaderT act adaptedValue

app_ :: ValidSelector acs n
  => SList acs
  -> QAct b (Length acs) a
  -> QAct b n ()
app_ sl act = app sl act >> return ()

appAll :: forall b n a. KnownNat n 
  => QAct b 1 a -> QAct b n (Vec n a)
appAll op = do
  qv <- ask
  liftIO $ unsafeCoerce $ sequence
    [runQ op $ unsafeCoerce (unsafeSelectQ (unsafeCoerce [ix]) qv)
      | ix <- [(1 :: Int)..fromIntegral $ natVal $ Proxy @n]]

appAll_ :: forall n b a. KnownNat n 
  => QAct b 1 a -> QAct b n ()
appAll_ op = appAll op >> return ()

mapp :: (ValidSelector acs n, KnownNat (Length acs)) 
  => SList acs -> QAct b 1 a -> QAct b n (Vec (Length acs) a)
mapp sl op = do
  app sl (appAll op)

mapp_ :: forall n acs b a. (KnownNat n, ValidSelector acs n, KnownNat (Length acs)) 
  => SList acs -> QAct b 1 a -> QAct b n ()
mapp_ sl op = do
  app_ sl (appAll op)

--[MEASURE]--------------------------------------------------------

sample :: (Basis b, Show b) => QAct b s ()
sample = do
  virt <- ask
  liftIO $ printQS virt

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

measureAll :: (Basis b, KnownNat n) => QAct b n (Vec n b)
measureAll = do
  qv <- ask
  k <- liftIO $ measureVirtAll qv
  return $ unsafeCoerce k

--[PARALLEL]--------------------------------------------------------


phaseOracle :: forall b n. (Basis b, Show b, KnownNat n) => (Vec n b -> Bool) -> QAct b n ()
phaseOracle f = do
  let op = mkOP @b
          [ ((b,b), if f $ unsafeCoerce b then -1 else 1)
           | b <- basis @b $ fromIntegral (natVal (Proxy @n))]
  vv <- ask
  liftIO $ appV op vv

(<-@>) :: forall n1 n2 n3 b a c. Partition n1 n2 n3
  => QAct b n1 a
  -> QAct b n2 c
  -> QAct b n3 (a,c)
act1 <-@> act2 = do
  let 
    midBound = fromIntegral $ natVal (Proxy @n1)
    upperBound = fromIntegral $ natVal (Proxy @n2) + natVal (Proxy @n1)  
  
    selectorsLeft  = [1 .. midBound]
    selectorsRight = [succ midBound .. upperBound]
  vv <- ask
  let
    adaptedLeft = unsafeSelectQInt @_ @n1 selectorsLeft vv
    adaptedRight = unsafeSelectQInt @_ @n2 selectorsRight vv

  a <- liftIO $ runQ act1 adaptedLeft
  b <- liftIO $ runQ act2 adaptedRight
  return (a,b)

(<@>) :: Partition n1 n2 n3 => QAct b n1 a -> QAct b n2 c -> QAct b n3 ()
a1 <@> a2 = (a1 <-@> a2) >> return ()

type Partition (n :: Natural) (m :: Natural) (k :: Natural) = (KnownNat n, KnownNat m, KnownNat k, n + m ~ k, m + n ~ k)