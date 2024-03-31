module Num where

instance Num a => Num (Maybe a) where
  (+) (Just el) (Just er) = Just $ el + er
  (+) _ _ = Nothing

  (*) (Just el) (Just er) = Just $ el * er
  (*) _ _ = Nothing

  abs = fmap abs
  signum = fmap signum
  fromInteger = Just . fromInteger
  negate = fmap negate
