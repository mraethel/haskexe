import ExprT

eval :: ExprT -> Integer
eval (Lit l) = l
eval (Add el er) = (eval el) + (eval er)
eval (Mul el er) = (eval el) * (eval er)

main = putStr . show . eval $ Mul (Add (Lit 2) (Lit 3)) (Lit 4)
