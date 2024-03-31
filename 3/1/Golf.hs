module Golf where

skips :: [a] -> [[a]]
skips xs = map (\i -> (snd . unzip . filter (\t -> mod (fst t) i == 0) . zip [1..]) xs) [1..(length xs)]
