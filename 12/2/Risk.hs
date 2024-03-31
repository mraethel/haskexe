{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

evalBattle :: (DieValue, DieValue) -> Battlefield -> Battlefield
evalBattle (a,d) b
  | a > d = Battlefield (attackers b) $ defenders b - 1
  | otherwise = Battlefield (attackers b - 1) $ defenders b

dice :: Int -> Rand StdGen [DieValue]
dice n = sort <$> sequence (replicate n die)

battle :: Battlefield -> Rand StdGen Battlefield
battle b = (\a d -> (foldr evalBattle b (zip a d))) <$> dice (attackers b) <*> dice (defenders b)
