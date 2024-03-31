module Golf where

localMaxima :: [Integer] -> [Integer]
localMaxima xs = ((\(a,b,c) -> b) . unzip3 . filter (\(x,y,z) -> x<y && z<y) . zip3 xs (drop 1 xs)) $ drop 2 xs
