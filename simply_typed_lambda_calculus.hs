module SimplyTypedLambdaCalculus where

import Test.Hspec

-- Here's the simply typed lambda calculus. We define some algebraic
-- data types, using the grammar definition itself as closely as we can.

-- The grammar is thus:
-- t ::= x                       variable
--     | λx:T1.t2                abstraction
--     | t1 t2                   application
--     | true                    constant true
--     | false                   constant false
--     | if t1 then t2 else t3   conditional
-- It's described here: http://www.cis.upenn.edu/~bcpierce/sf/Stlc.html

-- We ignore boolean and conditionals, and we enable type annotation by
-- supplying it as a parameter to lambdas.

-- Note: We're not going to be building an interpreter. We're working
-- with an abstract syntax tree here (AST), meaning the representation
-- of code after a parser reads data. Also, parsers are easy so we'll
-- skip writing one.

type Id = String

data Expr = Var Id
          | Lambda Id Type Expr
          | App Expr Expr
          | Base
          deriving (Show, Read, Eq)

data Type = TyBase
          | TyArr Type Type
          deriving (Show, Read, Eq)

-- We need some way of keeping track of how types are related to Id's,
-- so let's use a simple association list (list of pairs).
type Gamma = [(Id, Type)]

-- Now, how do we find an Id's type? Here's how:

lookupType :: Id -> Gamma -> Maybe Type

lookupType id [] = Nothing

lookupType id ((var,typ):rest)
  | id == var = Just typ
  | otherwise = lookupType id rest


-- Now, let's write a type checker. It needs to take a expr and a gamma,
-- and return either type or Nothing (where Nothing encompasses all
-- failure modes).

typeCheck :: Expr -> Gamma -> Maybe Type

typeCheck (Var x) gamma = lookupType x gamma

typeCheck (Lambda x typ body) gamma = do
  typ2 <- typeCheck body ((x, typ):gamma)
  return $ TyArr typ typ2

typeCheck (App e1 e2) gamma = do
  typ1 <- typeCheck e1 gamma
  typ2 <- typeCheck e2 gamma
  case typ1 of
    TyArr t1 t2 ->   if t1 == typ2
                     then Just t1
                     else Nothing
    _ -> Nothing
    
typeCheck Base gamma = Just TyBase

checkBehavior = hspec $ do
  describe "typeCheck" $ do

    describe "Base" $ do
      it "should always have TyBase type" $ do
        (typeCheck Base [("", TyBase)]) `shouldBe` Just TyBase
        (typeCheck Base []) `shouldBe` Just TyBase

    describe "Var" $ do
      it "should check if Var has type in Gamma" $ do
        (typeCheck (Var "x") []) `shouldBe` Nothing
        (typeCheck (Var "x") [("x", TyBase)]) `shouldBe` Just TyBase

    describe "Lambda" $ do
      it "should have type TyArr" $
        let lam = Lambda "x" TyBase (Var "x") in
        (typeCheck lam  []) `shouldBe` Just (TyArr TyBase TyBase)
        
      it "should evaluate to Nothing if there's a free Var" $
        let lam = (Lambda "x" TyBase (Var "y")) in
        (typeCheck lam []) `shouldBe` Nothing

      it "should have arrow type regardless of Gamma" $
        let lam = (Lambda "x" TyBase (Var "x")) in
        (typeCheck lam [("y", TyBase)]) `shouldBe` Just (TyArr TyBase TyBase)

    describe "App" $ do
      it "should evaluate to Nothing if input type of Lambda doesn't match second param" $
        let lam = (Lambda "x" TyBase (Var "x"))
            app = (App lam (Var "y"))
        in (typeCheck app []) `shouldBe` Nothing

      let lam = (Lambda "x" TyBase (Var "x"))
          app = (App lam (Var "y"))
      it "should check if input of lambda has same type as app's input" $ do
        (typeCheck app []) `shouldBe` Nothing
        (typeCheck app [("y", TyBase)]) `shouldBe` Just TyBase
