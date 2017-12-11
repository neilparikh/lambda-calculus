module SimplyTyped.TypeChecker where

import Data.Map.Strict as M

import SimplyTyped.Types

type Context = M.Map Name Type

emptyCtx :: Context
emptyCtx = M.empty

typeCheck :: Context -> Expr -> Type -> Bool
typeCheck ctx (Lambda name e) (Function t1 t2) = typeCheck (M.insert name t1 ctx) e t2
typeCheck _ (Lambda _ _ ) (Base _) = False
typeCheck ctx expr t = (typeInfer ctx expr) == (Just t)

typeInfer :: Context -> Expr -> Maybe Type
typeInfer _ (Lambda _ _) = Nothing
typeInfer ctx (App e1 e2) = case (typeInfer ctx e1) of
    Just (Function t1 t2) -> if (typeCheck ctx e2 t1) then (Just t2) else Nothing
    Just (Base _) -> Nothing
    Nothing -> Nothing
typeInfer ctx (Variable name) = M.lookup name ctx
