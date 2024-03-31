{-# LANGUAGE FlexibleInstances #-}

module HasVars where
import qualified Data.Map as M
import VarExprT

class HasVars a where
  var :: String -> a

instance HasVars VarExprT where
  var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup
