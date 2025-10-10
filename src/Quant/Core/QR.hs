{-# OPTIONS_GHC -Wno-x-partial #-}
module Core.QR
  ( QR
  , qrApp
  , printQR
  , showQR
  , module Core.OP
  , qrFromList
  , observeQR
  ) where

import Core.OP

import Data.IORef

type QR a = (IORef (QV a))

qrFromList :: Basis a => [([a], PA)] -> IO (QR a)
qrFromList lst = newIORef $ mkQV lst

qrApp :: Basis a => Ord a => OP a -> QR a -> IO ()
qrApp op ref = modifyIORef ref (appOP op)

showQR :: Show a => QR a -> IO String
showQR ref = do
  qval <- readIORef ref
  return $ show qval

printQR :: Show a => QR a -> IO ()
printQR ref = do
  qval <- readIORef ref
  print qval

observeQR :: Basis a => QR a -> Int -> IO a 
observeQR ref ix = do
  qVal <- readIORef ref
  let 
    wsize = qvSize qVal
    prob' a =
        sqrt . sum
          $ [ squareModulus
              (getProb qVal (left ++ a ++ right)) :+ 0
            | left <- basis (ix - 1)
            , right <- basis (wsize - ix)
            ]
  obsRes <- observeV $ mkQV [(a, prob' a) | a <- basis 1]
  let
    newVal =
      mkQV [( left ++ obsRes ++ right
              , getProb qVal (left ++ obsRes ++ right))
            | left <- basis (ix - 1)
            , right <- basis (wsize - ix)
            ] 
  writeIORef ref (normalize newVal)
  return $ head obsRes
