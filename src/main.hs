import qualified SimplyTyped.Parser as S (applyParser, exprParser, typeParser)
import qualified SimplyTyped.TypeChecker as S (typeCheck, emptyCtx)

main :: IO ()
main = do
    putStrLn "Which type system would you like to run?"
    putStrLn "- SimplyTyped"
    input <- getLine
    let (exprParse, typeParse, typeCheck, emptyCtx) = case input of
            "SimplyTyped" -> (S.applyParser S.exprParser, S.applyParser S.typeParser, S.typeCheck, S.emptyCtx)
            _ -> error "invalid option"
    let lambdaExpr = "(|x.(|y.(|z.(y(xz)))))"
    let lambdaType = "((A -> B) -> ((B -> C) -> (A -> C)))"
    let exprAndType = do
            expr <- exprParse lambdaExpr
            t <- typeParse lambdaType
            pure (expr, t)
    putStrLn $ case exprAndType of
        Right (expr, t) -> show expr ++ " : " ++ show t ++ " = " ++ show (typeCheck emptyCtx expr t)
        Left err -> show err

