module Inferer where

-- Here's the lambda calculus

data Expr = Var Id
          | Lambda Id Expr
          | App Expr Expr
          deriving (Show, Read, Eq)

type Id = String


-- you can play with the above:
--   Var "x" -> Var "x"
--   Lambda "x" (Var "e") -> Lambda "x" (Var "e")
--   App (Var "x") (Var "e") -> App (Var "x") (Var "e")




