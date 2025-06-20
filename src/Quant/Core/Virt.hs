module Core.Virt
  ( Virt(..)
  , module Core.QR
  , mkQ
  , printQ
  , unsafeSelectQ
  , appV
  , measureVirt
  , printQS
  , measureVirtN) where

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

printQ :: Show a => Virt a acs -> IO ()
printQ (Virt qr _) = do
  printQR qr

printQS :: (Basis a, Show a) => Virt a acs -> IO ()
printQS (Virt qr acs) = do
  qv <- readIORef qr
  let b = mkQV [(($ l) . flip (!!) <$> map pred acs, p) | (l, p) <- getEntries qv]
  print b

unsafeSelectQ ::
  forall nacs n a. SList nacs -> Virt a n -> Virt a (Length nacs)
unsafeSelectQ sl (Virt qr acs) = Virt qr (((acs !!) . pred) <$> sListToList sl)

measureVirt ::
    forall a s. Basis a
    => Virt a s -> Int -> IO a
measureVirt (Virt qr acs) ix = observeQR qr (acs !! (ix - 1))

measureVirtN ::
  forall a s. Basis a
    => Virt a s -> [Int] -> IO [a]
measureVirtN (Virt qr acs) ixs =
  sequence $ [observeQR qr (acs !! (ix - 1)) | ix <- ixs]

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