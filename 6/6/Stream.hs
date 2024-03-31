{-# OPTIONS_GHC -fno-warn-missing-methods #-}

data Stream a = Cons a (Stream a) deriving Functor

streamToList :: Stream a -> [a]
streamToList (Cons x s) = x : streamToList s

streamRepeat :: a -> Stream a
streamRepeat x = Cons x $ streamRepeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap = fmap

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x $ streamFromSeed f $ f x

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a s) t = Cons a (interleaveStreams t s)

recRuler :: Integer -> Stream Integer
recRuler i = interleaveStreams (streamRepeat i) $ recRuler $ i + 1

ruler :: Stream Integer
ruler = recRuler 0 

x :: Stream Integer
x = Cons 0 $ Cons 1 $ streamRepeat 0

instance Num (Stream Integer) where
  fromInteger n = Cons n $ streamRepeat 0
  negate = streamMap negate
  (+) (Cons a s) (Cons b t) = Cons (a + b) $ s + t
  (*) (Cons a s) bt@(Cons b t) = Cons (a * b) $ (+) (streamMap (*a) t) $ (*) s bt

instance Fractional (Stream Integer) where
  (/) as@(Cons a s) bt@(Cons b t) = Cons (div a b) $ streamMap (*(div 1 b)) $ (-) s $ (*) t $ (/) as bt

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

fibs3 :: Stream Integer
fibs3 = (/) x $ 1 - x - x^2
