module Golf where

import Data.List

histogram :: [Integer] -> String
histogram = unlines . reverse . (++) ["0123456789", "=========="] . map line . takeWhile (not . null) . iterate (\\ [0..9])

line :: [Integer] -> String
line xs = map (\i -> if elem i xs then '*' else ' ') [0..9]
