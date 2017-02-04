module Parser where
import Text.Parsec

import Common

type Parser a = Parsec String () a
applyParser :: Parser a -> String -> Either ParseError a
applyParser parser = runParser parser () ""

parens :: Parser a -> Parser a
parens subParser = char '(' *> subParser <* char ')'

exprParser :: Parser Expr
exprParser = parens (appParser <|> lambdaParser <|> variableParser)

lambdaParser :: Parser Expr
lambdaParser = Lambda <$> (char 'λ' *> letter) <*> (char '.' *> exprParser)

appParser :: Parser Expr
appParser = App <$> exprParser <*> exprParser

variableParser :: Parser Expr
variableParser = Variable <$> letter
