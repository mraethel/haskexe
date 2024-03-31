import JoinList
import Sized

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty Empty = Empty
(+++) Empty k = k
(+++) j Empty = j
(+++) j k = Append (tag j <> tag k) j k

intSize :: Sized a => a -> Int
intSize = getSize . size

tagSize :: (Sized b, Monoid b) => JoinList b a -> Int
tagSize = intSize . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ x (Single b a)
  | x+1 == intSize b = Just a
  | otherwise = Nothing
indexJ x (Append b j k)
  | x < tagSize j = indexJ x j
  | otherwise = indexJ (x - tagSize j) k

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ x j
  | x <= 0 = j
  | x >= tagSize j = Empty
  | otherwise = case j of
                  s@(Single _ _) -> s
                  Append _ k l -> dropJ x k +++ dropJ (x - tagSize k) l

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ x j
  | x <= 0 = Empty
  | x >= tagSize j = j
  | otherwise = case j of
                  s@(Single _ _) -> s
                  Append _ k l -> takeJ x k +++ takeJ (x - tagSize k) l
