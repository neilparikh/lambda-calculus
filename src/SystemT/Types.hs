module SystemT.Types where

type Name = Char

data Expr = Variable Name
          | Lambda Name Expr
          | App Expr Expr
          | Zero
          | Succ Expr
          | R Expr Expr Expr
          | F
          | T
          | D Expr Expr Expr
          deriving Eq


instance Show Expr where
    show (Variable name) = [name]
    show (Lambda name e) = "(" ++ '|':name:'.':[] ++ show e ++ ")"
    show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (R a b c) = "(R " ++ (show a) ++ " " ++ (show b) ++ " " ++ (show c) ++ ")"
    show Zero = "0"
    show n@(Succ x) = if (isNat n)
                      then show . natToInt $ n
                      else "(" ++ show (findNonSucc x) ++ " + " ++ (show . countSucc $ n) ++ ")"
        where
        isNat Zero = True
        isNat (Succ x') = isNat x'
        isNat _ = False
        natToInt :: Expr -> Int
        natToInt Zero = 0
        natToInt (Succ x') = 1 + natToInt x'
        natToInt _ = error "nat to int called with non nat"
        countSucc :: Expr -> Int
        countSucc (Succ n') = 1 + countSucc n'
        countSucc _ = 0
        findNonSucc (Succ x') = findNonSucc x'
        findNonSucc x' = x'
    show T = "True"
    show F = "False"
    show (D a b c) = "(D " ++ (show a) ++ " " ++ (show b) ++ " " ++ (show c) ++ ")"


data Type = Base Name
          | BaseNat
          | BaseBool
          | Function Type Type
          deriving Eq

instance Show Type where
    show (Base name) = [name]
    show BaseNat = "Nat"
    show BaseBool = "Bool"
    show (Function t1 t2) = "(" ++ (show t1) ++ " -> " ++ (show t2) ++ ")"
