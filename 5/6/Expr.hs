{-# LANGUAGE FlexibleInstances #-}

module Expr where
import qualified Data.Map as M
import VarExprT
import Num

class Expr t where
  lit :: Integer -> t
  add :: t -> t -> t
  mul :: t -> t -> t

instance Expr VarExprT where
  lit = Lit
  add = Add
  mul = Mul

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit k = \vs -> Just k
  add fl fr = \vs -> (fl vs) + (fr vs)
  mul fl fr = \vs -> (fl vs) * (fr vs)
