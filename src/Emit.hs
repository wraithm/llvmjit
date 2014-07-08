module Emit where

import           Control.Lens              (use)
import           Control.Monad             (forM_)
import           Control.Monad.Error

import           LLVM.General.AST          (Module (..), Name (..),
                                            Operand (..), Type)
import           LLVM.General.AST.Constant as C
import           LLVM.General.AST.Float    as F

import           LLVM.General.Context
import qualified LLVM.General.Module       as M

import           Codegen
import           JIT
import           Syntax                    hiding (Name)


toSig :: [String] -> [(Type, Name)]
toSig = map (\x -> (double, Name x))

codegenDefn :: Program -> Defn -> LLVM ()
codegenDefn prog (Function n args body) = define double n fnargs bls
  where
    fnargs = toSig args
    bls = createBlocks $ execCodegen $ do
        entry <- addBlock entryBlockName
        _ <- setBlock entry
        forM_ args $ \a -> do
            var <- alloca double
            _ <- store var (local (Name a))
            assign a var
        cgen prog body >>= ret
codegenDefn _ (Extern n args) = external double n fnargs
  where
    fnargs = toSig args

cgen :: Program -> Expr -> Codegen Operand
cgen prog (Var x) = do --getVar x >>= load -- lookup variable in program
    syms <- use symtab
    case lookup x syms of
        Just x -> return x
        Nothing -> lookupProg prog x
  where
    lookupProg :: Program -> String -> Codegen Operand
    lookupProg (Function n _ _:defs) x
        | n == x = return $ externf (Name x)
    lookupProg (Extern n _:defs) x
        | n == x = return $ externf (Name x)
    lookupProg (_:defs) x = lookupProg defs x
    lookupProg [] x = error "Cannot find symbol in program, nor local context..."
cgen _ (Num n) = return $ ConstantOperand $ Float (Double n)
cgen prog a@(App _ _) = do
    let (fn, args) = cgenApp a
    largs <- mapM (cgen prog) args
    call (externf (Name fn)) largs
cgen _ _ = error "Can't compile let or lam"

cgenApp :: Expr -> (String, [Expr])
cgenApp (App (Var n) e1) = (n, [e1])
cgenApp (App e1 e2) = (n, args ++ [e2])
  where (n, args) = cgenApp e1
cgenApp _ = error "Not app..."

liftError :: ErrorT String IO a -> IO a
liftError = runErrorT >=> either fail return

codegen :: Module -> Program -> IO Module
codegen mod fns = withContext $ \context ->
    liftError $ M.withModuleFromAST context newast $ \m -> do
        llstr <- M.moduleLLVMAssembly m
        putStrLn llstr
        return newast
  where
    modn = mapM (codegenDefn fns) fns
    newast = runLLVM mod modn

jitCodegen :: Module -> Program -> IO Module
jitCodegen mod fns = do
    res <- runJIT oldast
    case res of
        Right newast -> return newast
        Left err -> putStrLn err >> return oldast
  where
    modn = mapM (codegenDefn fns) fns
    oldast = runLLVM mod modn
