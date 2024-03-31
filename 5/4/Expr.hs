module Expr where

class Expr t where
  lit :: Integer -> t
  add :: t -> t -> t
  mul :: t -> t -> t

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (<) 0
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
  lit = MinMax
  add (MinMax el) (MinMax er) = lit $ max el er
  mul (MinMax el) (MinMax er) = lit $ min el er

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
  lit = Mod7 . flip mod 7
  add (Mod7 el) (Mod7 er) = lit $ el + er
  mul (Mod7 el) (Mod7 er) = lit $ el * er
