module QAct.QHistory(QRep(..), QHistory) where

data QRep = 
    FullCircle Int
  | CirclePlus Int
  | Cross Int
  | SimpleOp String Int
  | ClusterOP [QRep]
  | Measure Int
  | Sample String

instance Show QRep where
  show (FullCircle n) = "FullCircle " ++ show n
  show (CirclePlus n) = "CirclePlus " ++ show n
  show (Cross n) = "Cross " ++ show n
  show (SimpleOp s n) = "SimpleOp " ++ s ++ " " ++ show n
  show (ClusterOP lst) = "ClusterOP " ++ show lst
  show (Measure n) = "Measure " ++ show n
  show (Sample s) = "Sample " ++ s

type QHistory = [QRep]