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
  , module List.NList
  , module List.SList
  , module Core.Virt
  ) where

import Core.Virt
import List.NList
import List.SList

import Control.Monad.Reader
import Data.Kind
import Unsafe.Coerce
import Data.Data (Proxy(..))

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
  liftIO $ measureVirt virt (fromIntegral $ natVal sn)

measureN :: (Basis b, ValidSelector acs n) => SList acs -> QAct b n (NList b (Length acs))
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

phaseOracle :: forall b n. (Basis b, Show b, KnownNat n) => (NList b n -> Bool) -> QAct b n ()
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
      | ix <- [1..fromIntegral $ natVal $ Proxy @n]]

appAll_ :: forall b n a. KnownNat n => QAct b 1 a -> QAct b n ()
appAll_ op = do
  _ <- appAll op
  return ()


-- controlled :: forall b controls targets. (Basis b, ValidSelector (controls <++> targets) (Length (controls <++> targets)) )
--   => (NList b (Length controls) -> Bool) 
--   -> SList controls
--   -> SList targets
--   -> Matrix b (Length targets)
--   -> QAct b (Length (controls <++> targets)) ()
-- controlled = do 
--   let 
--     mat = unsafeCoerce mat :: [(([b],[b]), PA)]
--     controlCount = fromIntegral $ natVal (Proxy @control)
--     targetCount = fromIntegral $ natVal (Proxy @target)


-- oh damn....
-- controlled :: forall b control target. (Show b, Basis b, KnownNat control, KnownNat target) 
--   => (NList b control -> Bool) -> Matrix b target -> QAct b (control + target) ()
-- controlled f mat = do
--   let
--     mat' = unsafeCoerce mat :: [(([b],[b]), PA)]
--     controlCount = fromIntegral $ natVal (Proxy @control)
--     targetCount = fromIntegral $ natVal (Proxy @target)
--     unchangeCase = [((control ++ target, control ++ target), 1) |
--                       control <- basis @b controlCount,
--                       not (f $ unsafeCoerce control),
--                       target <- basis @b targetCount]
--     changeCase = [((control ++ target, control ++ outcome), getMatrixProb target outcome) |
--                       control <- basis @b controlCount,
--                       f $ unsafeCoerce control,
--                       target <- basis @b targetCount,
--                       outcome <- basis @b targetCount,
--                       getMatrixProb target outcome /= 0]
--     getMatrixProb t1 t2 = maybe 0 snd (find (\((a,b), _) -> a == t1 && b == t2) mat')
--     op = mkOP (changeCase ++ unchangeCase)
--   vv <- ask
--   liftIO $ print "alou" >> print op
--   liftIO $ appV op vv