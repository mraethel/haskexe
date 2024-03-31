import Expr
import ExprT
import Parser

eval :: ExprT -> Integer
eval (Lit l) = l
eval (Add el er) = (eval el) + (eval er)
eval (Mul el er) = (eval el) * (eval er)

main = putStr . show . eval $ mul (add (lit 2) (lit 3)) (lit 4)
