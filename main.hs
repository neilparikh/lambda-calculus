import Eval (eval)
import Parser (applyParser, exprParser)

main :: IO ()
main = do
    putStrLn $ case applyParser exprParser "(((|x.(|y.(xy)))(|a.a))r)" of
        Right expr -> show . eval $ expr
        Left error -> show error

