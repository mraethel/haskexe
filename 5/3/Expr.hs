module Expr where
import ExprT

class Expr t where
  lit :: Integer -> t
  add :: t -> t -> t
  mul :: t -> t -> t

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul
