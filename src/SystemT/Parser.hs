module SystemT.Parser where
import Text.Parsec

import SystemT.Types

type Parser a = Parsec String () a
applyParser :: Parser a -> String -> Either ParseError a
applyParser parser = runParser parser () ""

parens :: Parser a -> Parser a
parens subParser = char '(' *> subParser <* char ')'

constString :: String -> a -> Parser a
constString s f = string s *> return f

exprParser :: Parser Expr
exprParser = (parens (recurseParser <|> succParser <|> ifParser <|> appParser <|> lambdaParser)) <|> variableParser <|> natParser <|> boolParser

lambdaParser :: Parser Expr
lambdaParser = Lambda <$> (char '|' *> lower) <*> (char '.' *> exprParser)

appParser :: Parser Expr
appParser = App <$> exprParser <*> exprParser

variableParser :: Parser Expr
variableParser = Variable <$> lower

natParser :: Parser Expr
natParser = do
    raw <- many1 digit
    return (intToNat . read $ raw)
    where
    intToNat :: Int -> Expr
    intToNat 0 = Zero
    intToNat x
        | x > 0 = Succ (intToNat (pred x))
        | otherwise = error "negative nums not allowed"

succParser :: Parser Expr
succParser = do
    _ <- string "S "
    expr <- exprParser
    return $ Succ expr

boolParser :: Parser Expr
boolParser =     constString "T" T
             <|> constString "F" F

recurseParser :: Parser Expr
recurseParser = R <$> (char 'R' *> spaces *> exprParser)
                  <*> (spaces *> exprParser)
                  <*> (spaces *> exprParser)

ifParser :: Parser Expr
ifParser = D <$> (char 'D' *> spaces *> exprParser)
             <*> (spaces *> exprParser)
             <*> (spaces *> exprParser)

typeParser :: Parser Type
typeParser = try functionParser <|> parens functionParser <|> baseTypeParser

parameterTypeParser :: Parser Type
parameterTypeParser = parens functionParser <|> baseTypeParser

baseTypeParser :: Parser Type
baseTypeParser = try baseBoolParser <|> try baseNatParser <|> baseParser

baseParser :: Parser Type
baseParser = Base <$> upper

baseNatParser :: Parser Type
baseNatParser = constString "Nat" BaseNat

baseBoolParser :: Parser Type
baseBoolParser = constString "Bool" BaseBool

functionParser :: Parser Type
functionParser = Function <$> parameterTypeParser <* string " -> " <*> typeParser
