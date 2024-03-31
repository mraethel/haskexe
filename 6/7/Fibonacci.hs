{-# OPTIONS_GHC -fno-warn-missing-methods #-}

data Matrix = Cons Integer Integer
                   Integer Integer

instance Num Matrix where
  (*) (Cons a b c d) (Cons w x y z) = Cons (a*w + b*y) (a*x + b*z) (c*w + d*y) (c*x + d*z)

f :: Matrix
f = Cons 1 1 1 0

getFib :: Matrix -> Integer
getFib (Cons _ _ _ x) = x

fib4 :: Integer -> Integer
fib4 = getFib . (^) f . (+1)
