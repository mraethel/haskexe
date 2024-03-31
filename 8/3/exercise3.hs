import Employee
import Data.Tree

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es f) = GL (e:es) $ f + empFun e

instance Semigroup GuestList where
  (<>) (GL ea f) (GL eb g) = GL (ea ++ eb) $ f + g

instance Monoid GuestList where
  mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun glf@(GL _ f) glg@(GL _ g)
  | f < g = glg
  | otherwise = glf

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold = foldTree

nextLevel :: Employee -> [(GuestList,GuestList)] -> (GuestList,GuestList)
nextLevel b = (\(g,h) -> (glCons b h , g)) . foldMap id
