import qualified SimplyTyped.Parser as S (applyParser, exprParser, typeParser)
import qualified SimplyTyped.TypeChecker as S (typeCheck, emptyCtx)
import qualified SimplyTyped.Eval as S (eval)

import qualified SystemT.Parser as T (applyParser, exprParser, typeParser)
import qualified SystemT.TypeChecker as T (typeCheck, emptyCtx)
import qualified SystemT.Eval as T (eval)

main :: IO ()
main = do
    let lambdaExpr = "(|x.(|y.(|z.(y(xz)))))"
    let lambdaType = "((A -> B) -> ((B -> C) -> (A -> C)))"
    putStrLn "Which type system would you like to run?"
    putStrLn "- SimplyTyped"
    putStrLn "- SystemT"
    input <- getLine
    -- FIXME: find a way to get rid of this duplication
    case input of
        "SimplyTyped" -> do
            let exprAndType = do
                    expr <- S.applyParser S.exprParser lambdaExpr
                    t <- S.applyParser S.typeParser lambdaType
                    pure (expr, t)
            case exprAndType of
                Right (expr, t) -> do
                    putStrLn $ show expr ++ " : " ++ show t ++ " = " ++ show (S.typeCheck S.emptyCtx expr t)
                    putStrLn . show $ S.eval expr
                Left err -> putStrLn $ show err
        "SystemT" -> do
            let exprAndType = do
                    expr <- T.applyParser T.exprParser lambdaExpr
                    t <- T.applyParser T.typeParser lambdaType
                    pure (expr, t)
            case exprAndType of
                Right (expr, t) -> do
                    putStrLn $ show expr ++ " : " ++ show t ++ " = " ++ show (T.typeCheck T.emptyCtx expr t)
                    putStrLn . show $ T.eval expr
                Left err -> putStrLn $ show err
        _ -> error "invalid option"

