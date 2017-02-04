module Types where

type Name = Char

data Expr = Variable Name
          | Lambda Name Expr
          | App Expr Expr
          | Annotate Expr Type
          deriving Show

data Type = Base Name
          | Function Type Type
          deriving Show
