fib :: (Integer, Integer) -> (Integer, Integer) 
fib (x,y) = (y,x+y)

fibs2 :: [Integer]
fibs2 = map snd . iterate fib $ (1,0)
