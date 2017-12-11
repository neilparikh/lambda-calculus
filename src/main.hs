import SimplyTyped.Parser (applyParser, exprParser, typeParser)
import SimplyTyped.TypeChecker (typeCheck, emptyCtx)

main :: IO ()
main = do
    let lambdaExpr = "(|x.(|y.(|z.(y(xz)))))"
    let lambdaType = "((A -> B) -> ((B -> C) -> (A -> C)))"
    let exprAndType = do
            expr <- applyParser exprParser lambdaExpr
            t <- applyParser typeParser lambdaType
            pure (expr, t)
    putStrLn $ case exprAndType of
        Right (expr, t) -> show expr ++ " : " ++ show t ++ " = " ++ show (typeCheck emptyCtx expr t)
        Left err -> show err

