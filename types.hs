module Types where

type Name = Char

data Expr = Variable Name
          | Lambda Name Expr
          | App Expr Expr
          deriving Show

