module LambdaLift where

import Data.Set (Set, (\\))
import qualified Data.Set as S
import Data.List (mapAccumL, foldl')
import Data.Maybe (fromMaybe)

import Syntax

data AnnExpr' a
    = AVar Name
    | ANum Double
    | AApp AnnExpr AnnExpr
    | ALet [AnnDefn] AnnExpr
    | ALam [Name] AnnExpr

data AnnDefn = Func Name [Name] AnnExpr | Extn Name [Name]

type AnnExpr = (Set Name, AnnExpr' (Set Name))
type AnnProgram = [AnnDefn]

type NameSupply = Int

freeVarsOf :: AnnExpr -> Set Name
freeVarsOf = fst

initialNameSupply :: NameSupply
initialNameSupply = 0

getName :: NameSupply -> String -> (NameSupply, String)
getName ns prefix = (ns + 1, makeName prefix ns)

getNames :: NameSupply -> [String] -> (NameSupply, [String])
getNames ns prefixes = (ns + 1 + length prefixes, zipWith makeName prefixes [ns..])

makeName :: String -> NameSupply -> String
makeName prefix ns = prefix ++ "_" ++ show ns

lambdaLift :: Program -> Program
lambdaLift = collectCs . rename . abstract . freeVars

freeVars :: Program -> AnnProgram
freeVars = map defFreeVars
  where
    defFreeVars (Function n args body) = Func n args (freeVarsE (S.fromList args) body)
    defFreeVars (Extern n args) = Extn n args

bindersOf :: [Defn] -> [Name]
bindersOf = map binderOf
  where
    binderOf (Function n _ _) = n
    binderOf (Extern n _) = n

rhssOf :: [Defn] -> [Expr]
rhssOf = map rhsOf
  where
    rhsOf (Function _ _ e) = e
    rhsOf (Extern n _) = Var n

freeVarsE :: Set Name -> Expr -> AnnExpr
freeVarsE _ (Num d) = (S.empty, ANum d)
freeVarsE lv (Var x)
    | x `S.member` lv = (S.singleton x, AVar x)
    | otherwise = (S.empty, AVar x)
freeVarsE lv (App e1 e2) = (freeVarsOf e1' `S.union` freeVarsOf e2', AApp e1' e2')
  where
    e1' = freeVarsE lv e1
    e2' = freeVarsE lv e2
freeVarsE lv (Let defs body) = (defsFree `S.union` bodyFree, ALet defs' body')
  where
    defNames = bindersOf defs
    binders = S.fromList defNames
    body_lv = lv `S.union` binders
    rhss' = map (freeVarsE body_lv) (rhssOf defs)
    defs' = zipWith (\n rhs -> Func n [] rhs) defNames rhss'
    freeInVals = S.unions (map freeVarsOf rhss')
    defsFree = freeInVals \\ binders
    body' = freeVarsE body_lv body
    bodyFree = freeVarsOf body' \\ binders
freeVarsE lv (Lam args body) = (freeVarsOf body' \\ setArgs, ALam args body')
  where
    setArgs = S.fromList args
    lv' = lv `S.union` setArgs
    body' = freeVarsE lv' body

abstract :: AnnProgram -> Program
abstract = map defAbstract

defAbstract :: AnnDefn -> Defn
defAbstract (Func n args e) = Function n args (abstractE e)
defAbstract (Extn n args) = Extern n args

abstractE :: AnnExpr -> Expr
abstractE (_, AVar x) = Var x
abstractE (_, ANum n) = Num n
abstractE (_, AApp e1 e2) = App (abstractE e1) (abstractE e2)
abstractE (_, ALet defs body) = Let (map defAbstract defs) (abstractE body)
abstractE (free, ALam args body) = foldl' App c (map Var fvs)
  where
    fvs = S.toList free
    c = Let [Function "c" [] cRhs] (Var "c")
    cRhs = Lam (fvs ++ args) (abstractE body)

rename :: Program -> Program
rename = snd . mapAccumL renameC initialNameSupply

renameC :: NameSupply -> Defn -> (NameSupply, Defn)
renameC ns (Extern n args) = (ns', Extern n args')
    where
    (ns', args', _) = newNames ns args
renameC ns (Function n args rhs) = (ns'', Function n args' rhs')
    where
    (ns', args', env) = newNames ns args
    (ns'', rhs') = renameE env ns' rhs

newNames :: NameSupply -> [Name] -> (NameSupply, [Name], [(Name, Name)])
newNames ns oldNames = (ns', new, env)
  where
    (ns', new) = getNames ns oldNames
    env = zip oldNames new

renameE :: [(Name, Name)] -> NameSupply -> Expr -> (NameSupply, Expr)
renameE env ns (Var x) = (ns, Var (lookupDefault x x env))
  where
    lookupDefault d k = fromMaybe d . lookup k
renameE _ ns (Num n) = (ns, Num n)
renameE env ns (App e1 e2) = (ns'', App e1' e2')
  where
    (ns', e1') = renameE env ns e1
    (ns'', e2') = renameE env ns' e2
renameE env ns (Lam args body) = (ns'', Lam args' body')
  where
    (ns', args', env') = newNames ns args
    (ns'', body') = renameE (env' ++ env) ns' body
renameE env ns (Let defs body) = (ns''', Let defs' body')
  where
    (ns', body') = renameE bodyEnv ns body
    (ns'', binders', env') = newNames ns' (bindersOf defs)
    bodyEnv = env' ++ env
    (ns''', rhss') = mapAccumL (renameE bodyEnv) ns'' (rhssOf defs)
    defs' = zipWith (\n e -> Function n [] e) binders' rhss'


collectCs :: Program -> Program
collectCs = concatMap collectOne

collectOne :: Defn -> [Defn]
collectOne (Function n args rhs) = Function n args rhs' : cs
  where (cs, rhs') = collectCsE rhs
collectOne x = [x]

collectCsE :: Expr -> ([Defn], Expr)
collectCsE (App e1 e2) = (cs' ++ cs'', App e1' e2')
  where
    (cs', e1') = collectCsE e1
    (cs'', e2') = collectCsE e2
collectCsE (Lam args body) = (cs, Lam args body')
  where (cs, body') = collectCsE body
collectCsE (Let defs body) = (rhssCs ++ bodyCs ++ localCs, mkLet nonCs' body')
  where
    isLam (Lam _ _) = True
    isLam _ = False
    isLamRhs (Function _ _ e) = isLam e
    isLamRhs _ = False
    mkLet d b = if null d then b else Let d b

    (rhssCs, defs') = mapAccumL collectCsDef [] defs
    cs' = filter isLamRhs defs'
    nonCs' = filter (not . isLamRhs) defs'
    -- This is probably wrong
    localCs = [ Function n args b | Function n _ (Lam args b) <- cs' ]
    (bodyCs, body') = collectCsE body

    collectCsDef cs (Extern n args) = (cs, Extern n args)
    collectCsDef cs (Function n args rhs) = (cs ++ rhsCs, Function n args rhs')
      where (rhsCs, rhs') = collectCsE rhs
collectCsE x = ([], x)
