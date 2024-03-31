-- main = putStr . show . xor $ [False, True, False, False, True]
-- main = putStr . show . map (\b -> not b) $ [False, True, False, False, True]
main = putStr . show . myFoldl (\a b -> a + b) 0 $ take 100 [1..]

xor :: [Bool] -> Bool
xor ls = foldr (\x y -> if (x /= y) then True else False) False ls

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> (f x):y) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f x xs = foldr (\b a -> f a b) x $ reverse xs
