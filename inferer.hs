module LambdaCalculus where

-- Here's the lambda calculus:

type Id = String

data Expr = Var Id
          | Lambda Id Expr
          | App Expr Expr
          deriving (Show, Read, Eq)

-- you can play with the above:
--   Var "x" -> Var "x"
--   Lambda "x" (Var "e") -> Lambda "x" (Var "e")
--   App (Var "x") (Var "e") -> App (Var "x") (Var "e")
