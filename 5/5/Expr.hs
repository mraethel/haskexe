{-# LANGUAGE TypeSynonymInstances #-}

module Expr where
import StackVM

class Expr t where
  lit :: Integer -> t
  add :: t -> t -> t
  mul :: t -> t -> t

instance Expr Program where
  lit n = [ PushI n ];
  add pl pr = (++) pl $ (++) pr [ Add ]
  mul pl pr = (++) pl $ (++) pr [ Mul ]
