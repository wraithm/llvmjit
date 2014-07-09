module Syntax where

type Name = String

data Expr a
    = Var a
    | Num Double
    | App (Expr a) (Expr a)
    | Let [Defn a] (Expr a)
    | Lam [a] (Expr a)
    deriving (Show, Eq)

data Ty
    = TyNum
    | TyFun Ty Ty
    | TyVar Name
    deriving (Show, Eq)

data Defn a
    = Function Name [a] (Expr a)
--     | Extern Name [a] Ty
    deriving (Show, Eq)

type TyExpr = Expr (Name, Ty)
type TyProgram = [Defn (Name, Ty)]

type Program = [Defn Name]

bindersOf :: [Defn Name] -> [Name]
bindersOf = map binderOf
  where
    binderOf (Function n _ _) = n
--     binderOf (Extern n _) = n

rhssOf :: [Defn Name] -> [Expr Name]
rhssOf = map rhsOf
  where
    rhsOf (Function _ _ e) = e
--     rhsOf (Extern n _) = Var n

removeTypes :: TyExpr -> Expr Name
removeTypes (Var (n, _)) = Var n
removeTypes (Num n) = Num n
removeTypes (App e1 e2) = App (removeTypes e1) (removeTypes e2)
removeTypes (Let defs body) = Let (map removeDefTypes defs) (removeTypes body)
  where
    removeDefTypes (Function n args bd) = Function n (map fst args) (removeTypes bd)
--     removeDefTypes (Extern n args ty) = Extern n (map fst args) ty
removeTypes (Lam args body) = Lam (map fst args) (removeTypes body)

typeOf :: TyExpr -> Ty
typeOf (Var (_, ty)) = ty
typeOf (Num _) = TyNum
typeOf (App e1 e2) 
    | TyFun ty1 ty2 <- typeOf e1
    , ty3 <- typeOf e2
    , ty1 == ty3 = ty2
    | otherwise = error "Type error at function application!"
typeOf (Let _ body) = typeOf body
typeOf (Lam args body) = foldr TyFun bodyTy argTys
  where 
    argTys = map snd args
    bodyTy = typeOf body
