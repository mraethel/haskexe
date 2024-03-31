data Stream a = Cons a (Stream a) deriving Functor

streamToList :: Stream a -> [a]
streamToList (Cons x s) = x : streamToList s

streamRepeat :: a -> Stream a
streamRepeat x = Cons x $ streamRepeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap = fmap

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x $ streamFromSeed f $ f x

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList
