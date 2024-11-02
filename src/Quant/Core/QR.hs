module Core.QR
  ( mkQR
  , QR(..)
  , qrApp
  , printQR
  , showQR
  , module Core.OP
  , qrFromList
  , observeQR
  ) where

import Data.IORef
import Core.OP
import Control.Monad

newtype QR a =
  QR (IORef (QV a))

mkQR :: QV a -> IO (QR a)
mkQR qv = QR <$> newIORef qv

qrFromList :: Basis a => [([a], PA)] -> IO (QR a)
qrFromList lst = mkQR $ mkQV lst

qrApp :: Basis a => Ord a => OP a -> QR a -> IO ()
qrApp op (QR ref) = modifyIORef ref (appOP op)

showQR :: Show a => QR a -> IO String
showQR (QR ref) = do
  qval <- readIORef ref
  return $ show qval

printQR :: Show a => QR a -> IO ()
printQR (QR ref) = do
  qval <- readIORef ref
  putStrLn $ show qval

observeQR :: Basis a => QR a -> Int -> IO a 
observeQR (QR ptr) ix = do
  qVal <- readIORef ptr

  let 
    wholeSize = qvSize qVal
    prob' a =
        sqrt . sum
          $ [ squareModulus
              (getProb qVal (left ++ a ++ right))
              :+ 0
            | left <- basis (ix - 1)
            , right <- basis (wholeSize - ix)
            ]
    auxQval = mkQV [(a, prob' a) | a <- basis 1]

  if (ix <= 0 || ix > wholeSize) -- tirar isso daqui depois
    then error "Index out of bounds"
  else do
    [obsRes] <- observeV auxQval
    let 
      newVal =
        mkQV [( left ++ [obsRes] ++ right
                , getProb qVal (left ++ [obsRes] ++ right))
              | left <- basis (ix - 1)
              , right <- basis (wholeSize - ix)
              ] 
    writeIORef ptr (normalize newVal)
    return obsRes
