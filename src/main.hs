import SimplyTyped.Parser (applyParser, exprParser)
import SimplyTyped.TypeChecker (typeCheck, emptyCtx)
import SimplyTyped.Types (Expr(Annotate))

main :: IO ()
main = do
    let lambdaExpr = "((|x.(|y.(|z.(y(xz))))) : ((A -> B) -> ((B -> C) -> (A -> C))))"
    putStrLn $ case applyParser exprParser lambdaExpr of
        Right (Annotate expr t) -> show expr ++ " : " ++ show t ++ " = " ++ show (typeCheck emptyCtx expr t)
        Right _ -> show "to be implemented"
        Left err -> show err

