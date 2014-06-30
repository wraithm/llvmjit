module Syntax where

type Name = String

data Expr
    = Var Name
    | FlNum Double
    | App Expr Expr
    | Let [Defn] Expr
    | Lam [Name] Expr
    deriving (Show, Eq)

data Defn 
    = Function Name [Name] Expr
    | Extern Name [Name]
    deriving (Show, Eq)

type Program = [Defn]
