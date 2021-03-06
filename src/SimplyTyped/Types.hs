module SimplyTyped.Types where

type Name = Char

data Expr = Variable Name
          | Lambda Name Expr
          | App Expr Expr

instance Show Expr where
    show (Variable name) = [name]
    show (Lambda name e) = "(" ++ '|':name:'.':[] ++ show e ++ ")"
    show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"

data Type = Base Name
          | Function Type Type
          deriving Eq

instance Show Type where
    show (Base name) = [name]
    show (Function t1 t2) = "(" ++ (show t1) ++ " -> " ++ (show t2) ++ ")"
