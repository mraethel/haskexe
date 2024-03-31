import Data.List

main = putStr . show . sieveSundaram $ 100

sieveSundaram :: Integer -> [Integer]
sieveSundaram = \n -> map (\x -> 2*x+1) . (\\) [1..n] . filter (<=n) . map (\(i,j) -> i+j+2*i*j) . filter (\(i,j) -> i <= j) $ cartProd [1..n] [1..n]

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]
