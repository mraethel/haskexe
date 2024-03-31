main = putStr . show . foldTree $ "ABCD"

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = blength . foldr btree Leaf

btree :: a -> Tree a -> Tree a
btree a Leaf           = Node 0 Leaf a Leaf
btree a (Node x s b t) = Node (x+1) (btree a t) b s

blength :: Tree a -> Tree a
blength Leaf = Leaf
blength (Node x s a t) = Node (floor . logBase 2.0 . fromIntegral $ x+1) (blength s) a (blength t)

-- blength (Node x s a t) = Node (blist x) (blength s) a (blength t)
-- blist :: Integer -> Integer
-- blist x = (!!) (concat . map (\i -> take (2^i) . repeat $ i) $ [0..]) $ fromIntegral x
