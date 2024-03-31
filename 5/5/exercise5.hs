import Expr
import Parser
import StackVM

compile :: String -> Maybe Program
compile = parseExp lit add mul

main = putStr . show . fmap stackVM . compile $ "(3 * -4) + 5"
