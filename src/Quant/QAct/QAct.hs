{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use void" #-}
{-# LANGUAGE LinearTypes #-}
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
  , (<|||)
  , (|||)
  , phaseOracle
  , module List.Vec
  , module List.SList
  , module Core.Virt
  , Partition
  , liftQ
  , control
  , orc
  , controlM
  , lmap
  , matrixBuilder
  , printMatrix
  , matrixToList
  ) where

import Core.Virt
import List.Vec
import List.SList

import Control.Monad.Reader
import Data.Kind
import Unsafe.Coerce
import Data.Data (Proxy(..))
import Data.Maybe (fromMaybe)
import Data.List (intercalate)

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

(<|||) :: forall n1 n2 n3 b a c. Partition n1 n2 n3
  => QAct b n1 a
  -> QAct b n2 c
  -> QAct b n3 (a,c)
act1 <||| act2 = do
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

(|||) :: Partition n1 n2 n3 => QAct b n1 a -> QAct b n2 c -> QAct b n3 ()
a1 ||| a2 = (a1 <||| a2) >> return ()

liftQ :: forall a n. (KnownNat n, Basis a) => (Vec n a %1 -> Vec n a) -> QAct a n ()
liftQ op = qActMatrix $ [unsafeCoerce ((a, op (unsafeCoerce a)), 1)
                  | a <- basis @a $ natVal (Proxy @n)]

--[CONTROLLS]--------------------------------------------------------

lmap :: (a %1 -> a) -> Vec n a %1 -> Vec n a
lmap _ VNil = VNil
lmap f (v:>vs) = f v :> lmap f vs

control :: forall a n m k. (Partition n m k, Show a, Basis a)
  => (Vec n a -> Bool) -> (Vec m a %1 -> Vec m a) -> QAct a k ()
control enable f
  = do
  let op = [((a ++ b, a ++ unsafeCoerce (unsafeCoerce f $ unsafeCoerce b)), 1)
            | a <- basis @a $ natVal (Proxy @n)
            , enable $ unsafeCoerce a
            , b <- basis @a $ natVal (Proxy @m)]
          ++
          [((a ++ b, a ++ b), 1)
            | a <- basis @a $ natVal (Proxy @n)
            , not . enable $ unsafeCoerce a
            , b <- basis @a $ natVal (Proxy @m)] :: [(([a],[a]), PA)]
  qActMatrix (unsafeCoerce op :: Matrix k s)

controlM :: forall a n m k. (Partition n m k, Show a, Basis a)
  => (Vec n a -> Bool) -> Matrix m a -> QAct a k ()
controlM enable f = qActMatrix $ controlled enable f

controlled :: forall controls targs a. (Basis a, KnownNat controls, KnownNat targs)
  => (Vec controls a -> Bool) -> Matrix targs a -> Matrix (controls + targs) a
controlled enable op =
  let change = [ ((c ++ t, c ++ t), prb)
                | c <- basis @a (natVal (Proxy @controls))
                , enable $ unsafeCoerce c
                , t <- basis @a (natVal (Proxy @targs))
                , t' <- basis @a (natVal (Proxy @targs))
                , let prb = case filter (\(a,_) -> a == unsafeCoerce (t,t')) op of
                              [] -> 0
                              (_, a):_ -> a ]
      unchange = [ ((c ++ t, c ++ t), 1)
                 | c <- basis @a (natVal (Proxy @controls))
                 , not $ enable $ unsafeCoerce c
                 , t <- basis @a (natVal (Proxy @targs)) ]
  in unsafeCoerce $ change ++ unchange

phaseOracle :: forall b n. (Basis b, Show b, KnownNat n) => (Vec n b -> Bool) -> QAct b n ()
phaseOracle f = do
  let op = mkOP @b
          [ ((b,b), if f $ unsafeCoerce b then -1 else 1)
           | b <- basis @b $ natVal (Proxy @n)]
  vv <- ask
  liftIO $ appV op vv

orc :: forall n a. (KnownNat n, Show a, Basis a)
  => (Vec n a -> Bool) -> (Vec n a %1 -> Vec n a) -> QAct a n ()
orc enable f = do
  let op1 = [((unsafeCoerce a, r), 1)
            | a <- basis @a $ natVal (Proxy @n)
            , enable $ unsafeCoerce a
            , let r = unsafeCoerce f $ unsafeCoerce a] :: [((Vec n a, Vec n a), PA)]
      op2 =
            [((unsafeCoerce a, unsafeCoerce a), 1)
            | a <- basis @a $ natVal (Proxy @n)
            , not $ enable $ unsafeCoerce a] :: [((Vec n a, Vec n a), PA)]
  liftIO $ print op1
  liftIO $ print op2
  qActMatrix $ unsafeCoerce (op1 ++ op2)


-----------------------------------------------------------------------------------

matrixBuilder :: forall n b. (KnownNat n, Basis b) => (Int -> Int -> PA) -> Matrix n b
matrixBuilder f =
  let sz = fromIntegral $ natVal $ Proxy @n
      bas = basis @b sz
      range = [0..pred(2^sz)]
  in [((unsafeCoerce (bas !! j), unsafeCoerce (bas !! k)), f j k) | j <- range, k <- range] :: [((Vec n b, Vec n b), PA)]

matrixToList :: forall b s. (KnownNat s, Basis b) => Matrix s b -> [[PA]]
matrixToList m =
  let size = fromIntegral $ natVal $ Proxy @s
  in [ [ lookupEntry (unsafeCoerce i) (unsafeCoerce j) | j <- basis @b size ] | i <- basis @b size ]
  where
    lookupEntry i j = fromMaybe zero (lookup (i, j) m)

    zero = head [v | ((_, _), v) <- m] `seq` 0

printMatrix :: forall s b. (Basis b, KnownNat s) => Matrix s b -> IO ()
printMatrix m = putStrLn $ concatMap (intercalate "\t" . (\s -> s ++ ["\n"])) listOfStr
  where
    listOfStr = fmap showPA <$> matrixToList m