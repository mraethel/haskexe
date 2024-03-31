import ExprT
import Parser

eval :: ExprT -> Integer
eval (Lit l) = l
eval (Add el er) = (eval el) + (eval er)
eval (Mul el er) = (eval el) * (eval er)

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul

main = putStr . show . evalStr $ "(2+3)*4"
