module SystemT.TypeChecker where
-- is currently System T without products

import qualified Data.Map.Strict as M

import SystemT.Types

type Context = M.Map Name Type

emptyCtx :: Context
emptyCtx = M.empty

typeCheck :: Context -> Expr -> Type -> Bool
-- lambda
typeCheck ctx (Lambda name e) (Function t1 t2) = typeCheck (M.insert name t1 ctx) e t2
typeCheck _ (Lambda _ _ ) (Base _) = False
-- Nats
typeCheck _ Zero     t = t == BaseNat
typeCheck ctx (Succ n) t = t == BaseNat && (typeCheck ctx n BaseNat)
-- recursion operator
typeCheck ctx (R a b c) t = and $ zipWith (typeCheck ctx) [a, b, c] [t, Function t (Function BaseNat t), BaseNat]
-- bools
typeCheck _ T t = t == BaseBool
typeCheck _ F t = t == BaseBool
-- ifThenElse operator
typeCheck ctx (D a b c) t = and $ zipWith (typeCheck ctx) [a, b, c] [t, t, BaseBool]
-- other
typeCheck ctx expr t = (typeInfer ctx expr) == (Just t)

typeInfer :: Context -> Expr -> Maybe Type
typeInfer _ (Lambda _ _) = Nothing
typeInfer ctx (App e1 e2) = case (typeInfer ctx e1) of
    Just (Function t1 t2) -> if (typeCheck ctx e2 t1) then (Just t2) else Nothing
    Just (Base _) -> Nothing
    Just (BaseNat) -> Nothing
    Just (BaseBool) -> Nothing
    Nothing -> Nothing
typeInfer _ (Zero) = Just BaseNat
typeInfer ctx (Succ n) = if (typeInfer ctx n) == Just BaseNat
                         then Just BaseNat
                         else error "succ with a non-nat inside"
typeInfer ctx (R a b c) = do
    tA <- typeInfer ctx a
    tB <- typeInfer ctx b
    tC <- typeInfer ctx c
    if tC == BaseNat && tB == Function tA (Function BaseNat tA)
    then return tA
    else Nothing
typeInfer _ T = Just BaseBool
typeInfer _ F = Just BaseBool
typeInfer ctx (D a b c) = do
    tA <- typeInfer ctx a
    tB <- typeInfer ctx b
    tC <- typeInfer ctx c
    if tC == BaseBool && tA == tB then return tA else Nothing
typeInfer ctx (Variable name) = M.lookup name ctx
