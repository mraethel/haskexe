import qualified Data.Map as M
import Expr
import HasVars

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs

main = putStr . show . withVars [("x", 6)] $ add (lit 3) (var "x")
