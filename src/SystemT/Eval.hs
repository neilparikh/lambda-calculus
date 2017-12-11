module SystemT.Eval where
import SystemT.Types

eval :: Expr -> Expr
eval (Variable x) = Variable x
eval (Lambda x body) = Lambda x body
eval (App e1 e2) = case (eval e1) of
    Lambda x body -> eval (substitute body x e2)
    result -> App result e2
eval Zero = Zero
eval (Succ n) = Succ n
eval (R u _ Zero) = eval u
eval (R u v (Succ n)) = eval (App (App v (eval (R u v n))) n)
eval (R u v x) = eval (R u v (eval x ))
eval T = T
eval F = F
eval (D u v t)
    | (eval t) == T = u
    | (eval t) == F = v
    | otherwise = error "non-bool in an if expression"

substitute :: Expr -> Name -> Expr -> Expr
substitute (Variable a) b expr
    | a == b = expr
    | otherwise = (Variable a)
substitute (Lambda a body) b expr
    | a == b = (Lambda a body)
    | otherwise = Lambda a (substitute body b expr)
substitute (App e1 e2) b expr = App (subHelper e1) (subHelper e2)
    where
    subHelper baseExpr = substitute baseExpr b expr
substitute Zero _ _ = Zero
substitute (Succ a) name expr = Succ $ substitute a name expr
substitute (R u v x) name expr = R (substitute u name expr) (substitute v name expr) (substitute x name expr)
substitute T _ _ = T
substitute F _ _ = F
substitute (D u v x) name expr = D (substitute u name expr) (substitute v name expr) (substitute x name expr)
