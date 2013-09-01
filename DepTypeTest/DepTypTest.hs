{-# LANGUAGE GADTs
  , StandaloneDeriving
  , ExistentialQuantification
  , ViewPatterns
  , RankNTypes
  , DataKinds
  , TypeOperators
  , ScopedTypeVariables
  #-}

import Data.Type.Natural
import Data.Bifunctor
import Data.Vector.Sized (Vector(..))
import qualified Data.Vector.Sized as Vector

import Control.Monad
import Control.Applicative

import Proof.Equational as E

deriving instance Show (Leq n m)

type n `Le`  m = S n `Leq` m
type n `Geq` m = m   `Leq` n
type n `Ge`  m = m   `Le`  n

data HNat = forall n. SingI n => HNat (SNat n)

deriving instance Show HNat

instance Read HNat where
    readsPrec p s = do (n :: Integer, s) <- readsPrec p s
                       guard (0 <= n)
                       return (toHNat . intToNat $ n, s)

toHNat :: Nat -> HNat
toHNat = to $ HNat SZ
    where
      to h Z = h
      to (HNat n) (S m) = to (HNat $ SS n) m

compareNat :: SNat n -> SNat m -> (n `Leq` m) `Either` (n `Ge` m)
compareNat SZ m =  Left . ZeroLeq $ m
compareNat (SS n) SZ = Right . SuccLeqSucc . ZeroLeq $ n
compareNat (SS n) (SS m) = bimap SuccLeqSucc SuccLeqSucc $ compareNat n m

eqNat :: SNat n -> SNat m -> Maybe (n :=: m)
eqNat SZ SZ = Just Refl
eqNat (SS n) (SS m) = cong Proxy <$> eqNat n m
eqNat _ _ = Nothing

testLeq :: Nat -> Nat -> String
testLeq (toHNat -> HNat n) (toHNat -> HNat m) = show $ compareNat n m

castVector :: n :=: m -> Vector a n -> Vector a m
castVector Refl = id

readVector :: Read a => SNat n -> String -> Maybe (Vector a n)
readVector n = Vector.fromList n . map read . words

testIO1 :: IO ()
testIO1 = do
  HNat n <- read <$> getLine
  Just v1 <- readVector n <$> getLine
  Just v2 <- readVector n <$> getLine
  print $ Vector.zipWithSame (+) v1 v2

testIO2 :: IO ()
testIO2 = do
  HNat n1 <- read <$> getLine
  Just v1 <- readVector n1 <$> getLine
  HNat n2 <- read <$> getLine
  Just v2 <- readVector n2 <$> getLine
  let Just eq = n2 `eqNat` n1
      v2' = castVector eq v2
  print $ Vector.zipWithSame (+) v1 v2'
