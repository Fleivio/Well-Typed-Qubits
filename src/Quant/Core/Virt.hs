module Core.Virt
  ( Virt(..)
  , module Core.QR
  , mkQ
  , printQ
  , printQS
  , unsafeSelectQ
  , appV
  , measureVirt
  , measureVirtN
  , measureVirtAll
  , unsafeSelectQInt
  , getQV
  , unsafeFromQV) where

import List.Vec
import List.SList
import Core.QR

import Data.IORef
import Data.Kind
import Data.Proxy
import Unsafe.Coerce
import Data.List ((\\))

type Virt :: Type -> Natural -> Type

data Virt a n where
  Virt :: QR a -> [Int] -> Virt a n

mkQ :: forall s a. (KnownNat s, Basis a) => [(Vec s a, PA)] -> IO (Virt a s)
mkQ list = do
  qr <- qrFromList $ unsafeCoerce list
  return $ Virt qr [1..fromIntegral $ natVal (Proxy @s)]

unsafeFromQV :: forall n a. (Basis a) => QV a -> Int -> IO (Virt a n)
unsafeFromQV qv size = do
  qr <- newIORef qv
  return $ Virt qr [1..size] 

getQV :: Virt a n -> IO (QV a)
getQV (Virt qr _) = readIORef qr

printQ :: Show a => Virt a acs -> IO ()
printQ (Virt qr _) = do
  printQR qr

printQS :: (Basis a, Show a) => Virt a acs -> IO ()
printQS (Virt qr acs) = do
  qv <- readIORef qr
  let b = mkQV [(($ l) . flip (!!) <$> map pred acs, p) | (l, p) <- getEntries qv]
  print b

unsafeSelectQInt :: forall n m a. [Int] -> Virt a n -> Virt a m
unsafeSelectQInt sl (Virt qr acs) = Virt qr ((acs !!) . pred <$> sl)

unsafeSelectQ ::
  forall nacs n a. SList nacs -> Virt a n -> Virt a (Length nacs)
unsafeSelectQ sl (Virt qr acs) = Virt qr ((acs !!) . pred <$> sListToList sl)

measureVirtAll :: Basis a => Virt a s -> IO [a]
measureVirtAll (Virt qr acs) = sequence [observeQR qr ix | ix <- acs]

measureVirt ::
    forall a s. Basis a
    => Virt a s -> Int -> IO a
measureVirt (Virt qr acs) ix = observeQR qr (acs !! pred ix)

measureVirtN ::
  forall a s. Basis a
    => Virt a s -> [Int] -> IO [a]
measureVirtN (Virt qr acs) ixs =
  sequence $ [observeQR qr (acs !! pred ix) | ix <- ixs]

appV ::
     forall a s. Basis a
  => OP a -> Virt a s -> IO ()
appV f' (Virt ref acs) = do
  qv <- readIORef ref
  let
    op = gf $ qvSize qv
    fqv = appOP op qv
  writeIORef ref fqv
  where
    gf s =
      mkOP
        [ ((ua, ub), getOpProb f' (a, b))
        | ua <- basis s
        , ub <- basis s
        , let (a, na) = decompose acs ua
              (b, nb) = decompose acs ub
        , na == nb
        ]

decompose :: Eq a => [Int] -> [a] -> ([a], [a])
decompose acs' as =
  let
    asZ = zip [(1 :: Int)..] as
    selected = [asZ !! pred i | i <- acs']
    rest = asZ \\ selected
  in (snd <$> selected, snd <$> rest)