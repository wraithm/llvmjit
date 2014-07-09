module TypeInference where

import           Control.Monad.State

import           Data.List           (foldl', foldl1')
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Maybe          (fromMaybe)
import           Data.Set            (Set, (\\))
import qualified Data.Set            as S

import           Syntax

data Scheme = Scheme [Name] Ty
type Subst = Map Name Ty
newtype TypeEnv = TypeEnv (Map Name Scheme)

nullSubst :: Subst
nullSubst = M.empty

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = M.map (apply s1) s2 `M.union` s1

class Types a where
    freeTypeVars :: a -> Set String
    apply :: Subst -> a -> a

instance Types Ty where
    freeTypeVars (TyVar n) = S.singleton n
    freeTypeVars (TyFun t1 t2) = freeTypeVars t1 `S.union` freeTypeVars t2
    freeTypeVars _ = S.empty

    apply s (TyVar n) = fromMaybe (TyVar n) (M.lookup n s)
    apply s (TyFun t1 t2) = TyFun (apply s t1) (apply s t2)
    apply _ t = t

instance Types Scheme where
    freeTypeVars (Scheme vars t) = freeTypeVars t \\ S.fromList vars
    apply s (Scheme vars t) = Scheme vars (apply (foldr M.delete s vars) t)

instance Types a => Types [a] where
    freeTypeVars = foldr (S.union . freeTypeVars) S.empty
    apply s = map (apply s)

remove :: TypeEnv -> Name -> TypeEnv
remove (TypeEnv env) var = TypeEnv (M.delete var env)

instance Types TypeEnv where
    freeTypeVars (TypeEnv env) = freeTypeVars (M.elems env)
    apply s (TypeEnv env) = TypeEnv (M.map (apply s) env)

generalize :: TypeEnv -> Ty -> Scheme
generalize env t = Scheme vars t
  where vars = S.toList (freeTypeVars t \\ freeTypeVars env)

type TIState = Int
type TypeInference = StateT TIState IO

runTypeInference :: TypeInference a -> IO a
runTypeInference = flip evalStateT 0

incSupply :: TypeInference ()
incSupply = modify (+1)

newTyVar :: Name -> TypeInference Ty
newTyVar prefix = do
    s <- get
    incSupply
    return $ TyVar (prefix ++ show s)

instantiate :: Scheme -> TypeInference Ty
instantiate (Scheme vars t) = do
    nvars <- mapM (const (newTyVar "a")) vars
    let s = M.fromList (zip vars nvars)
    return $ apply s t

varBind :: Name -> Ty -> TypeInference Subst
varBind u t
    | t == TyVar u = return nullSubst
    | u `S.member` freeTypeVars t = error $ show u ++ " is not free!"
    | otherwise = return $ M.singleton u t

mostGeneralUnifier :: Ty -> Ty -> TypeInference Subst
mostGeneralUnifier (TyFun l r) (TyFun l' r') = do
    s1 <- mostGeneralUnifier l l'
    s2 <- mostGeneralUnifier (apply s1 r) (apply s1 r')
    return $ s1 `composeSubst` s2
mostGeneralUnifier (TyVar u) t = varBind u t
mostGeneralUnifier t (TyVar u) = varBind u t
mostGeneralUnifier TyNum TyNum = return nullSubst
mostGeneralUnifier t1 t2 = error $ "Could not unify " ++ show t1 ++ " and " ++ show t2

typeInference :: TypeEnv -> Expr Name -> TypeInference (Subst, Ty)
typeInference (TypeEnv env) (Var n) = case M.lookup n env of
    Nothing -> error $ "Unbound variable: " ++ n
    Just s -> do
        t <- instantiate s
        return (nullSubst, t)
typeInference _ (Num _) = return (nullSubst, TyNum)
typeInference env (Lam args body) = do
    tvars <- mapM newTyVar (take numArgs allStrings)
    let TypeEnv env' = foldl' remove env args
        argSchemes = M.fromList (zip args (map (Scheme []) tvars))
        env'' = TypeEnv (env' `M.union` argSchemes)
    (s1, t1) <- typeInference env'' body
    return (s1, foldr (TyFun . apply s1) t1 tvars)
  where
    numArgs = length args
    allStrings = [ c : s | s <- "" : allStrings, c <- ['a'..'z'] ]
typeInference env (App e1 e2) = do
    tv <- newTyVar "a"
    (s1, t1) <- typeInference env e1
    (s2, t2) <- typeInference (apply s1 env) e2
    s3 <- mostGeneralUnifier (apply s2 t1) (TyFun t2 tv)
    return (s3 `composeSubst` s2 `composeSubst` s1, apply s3 tv)
typeInference env (Let defs body) = do
    defTys <- mapM (typeInferenceDefn env) defs
    let s = foldl1' composeSubst (map fst defTys)
        TypeEnv env' = env
        tys = map (generalize (apply s env) . snd) defTys
        env'' = TypeEnv (M.fromList (zip defNames tys ++ M.toList env'))
    (s2, t2) <- typeInference (apply s env'') body
    return (s `composeSubst` s2, t2)
  where
    defNames = bindersOf defs

typeInferenceDefn :: TypeEnv -> Defn Name -> TypeInference (Subst, Ty)
typeInferenceDefn env (Function _ args body) = tiArgs env body args
-- typeInferenceDefn _ (Extern _ _ ty) = return (nullSubst, ty)

tiArgs :: TypeEnv -> Expr Name -> [Name] -> TypeInference (Subst, Ty)
tiArgs env body [] = typeInference env body
tiArgs env body (x:xs) = do
    (s, t) <- typeInference env (Var x)
    let TypeEnv env' = remove env x
        t'' = generalize (apply s env) t
        env'' = TypeEnv (M.insert x t'' env')
    (s', t') <- tiArgs env'' body xs
    return (s `composeSubst` s', TyFun t t')

inferType :: TypeEnv -> Expr Name -> TypeInference Ty
inferType env e = do
    (s, t) <- typeInference env e
    return (apply s t)

execTypeInference :: Expr Name -> IO ()
execTypeInference e = runTypeInference (inferType (TypeEnv M.empty) e) >>= print
