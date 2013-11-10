module SimplyTypedLambdaCalculus where

-- Here's the simply typed lambda calculus

type Id = String

data Expr = Var Id
            | Lambda Id Type Expr
            | App Expr Expr
            | Base
            deriving (Show, Read, Eq)

type Ctx = [(Id, Type)]

data Type = TyBase
           | TyArr Type Type
           deriving (Show, Read, Eq)

lookupType :: Id -> Ctx -> Maybe Type
lookupType id [] = Nothing
lookupType id ((var,typ):rest) = if id == var
                                 then Just typ
                                 else lookupType id rest


-- now, let's do a type checker

-- it needs to take a expr and a context

typeCheck (Var x) ctx = lookupType x ctx
typeCheck (Lambda x typ body) ctx = do
  typ2 <- typeCheck body ((x, typ):ctx)
  return $ TyArr typ typ2
typeCheck (App e1 e2) ctx = do
  typ1 <- typeCheck e1 ctx
  typ2 <- typeCheck e2 ctx
  case typ1 of
    TyArr t1 t2 ->   if t1 == typ2
                     then Just t1
                     else Nothing
    _ -> Nothing
typeCheck Base ctx = Just TyBase

                                  
-- tc1 = typeCheck (TApp (TLambda "x" TyBase (TVar "x")) (TVar "y")) [("y", TyBase)] == Just TyBase
-- tc2 = typeCheck (TApp (TLambda "x" TyBase (TVar "x")) (TVar "y")) [] == Nothing
-- tc3 = typeCheck (TLambda "x" TyBase (TVar "x")) [] == Just (TyArr TyBase TyBase)
-- tc4 = typeCheck (TVar "x") [] == Nothing
-- tc5 = typeCheck TBase [] == Just TyBase
-- tc6 = typeCheck TBase [("", TyBase)] == Just TyBase
