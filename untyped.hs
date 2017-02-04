import Common
import Parser (applyParser, exprParser)

main :: IO ()
main = do
    putStrLn $ case applyParser exprParser "(((λx.(λy.((x)(y))))(λa.(a)))(r))" of
        Right expr -> show . eval $ expr
        Left error -> show error

eval :: Expr -> Expr
eval (Variable x) = Variable x
eval (Lambda x body) = Lambda x body
eval (App e1 e2) = case (eval e1) of
    Lambda x body -> eval (substitute body x e2)
    result -> App result e2

substitute :: Expr -> Name -> Expr -> Expr
substitute (Variable a) b expr
    | a == b = expr
    | otherwise = (Variable a)
substitute (Lambda a body) b expr
    | a == b = (Lambda a body)
    | otherwise = Lambda a (substitute body b expr)
substitute (App e1 e2) b expr = App (subHelper e1) (subHelper e2)
    where
    subHelper baseExpr = substitute baseExpr b expr
