module JIT where

import           Foreign.Ptr                  (FunPtr, castFunPtr)

import           Control.Monad.Error

import           LLVM.General.AST as AST
import           LLVM.General.Context
import qualified LLVM.General.ExecutionEngine as EE
import           LLVM.General.Module
import           LLVM.General.PassManager
import           LLVM.General.Analysis

foreign import ccall "dynamic" haskFun :: FunPtr (IO Double) -> IO Double

run :: FunPtr a -> IO Double
run fn = haskFun (castFunPtr fn :: FunPtr (IO Double))

jit :: Context -> (EE.MCJIT -> IO a) -> IO a
jit c = EE.withMCJIT c optlevel model ptrelim fastins
  where
    optlevel = Just 0
    model = Nothing
    ptrelim = Nothing
    fastins = Nothing

passes :: PassSetSpec
passes = defaultCuratedPassSetSpec { optLevel = Just 3 }

runJIT :: AST.Module -> IO (Either String AST.Module)
runJIT modl =
    withContext $ \context ->
        jit context $ \execEngine ->
            runErrorT $ withModuleFromAST context modl $ \m ->
                withPassManager passes $ \pm -> do
                    _ <- runErrorT $ verify m
                    _ <- runPassManager pm m
                    optmod <- moduleAST m
                    s <- moduleLLVMAssembly m
                    putStrLn s

                    EE.withModuleInEngine execEngine m $ \ee -> do
                        mainfn <- EE.getFunction ee (Name "main")
                        case mainfn of
                            Just fn -> do
                                res <- run fn
                                putStrLn $ "Evaluated to: " ++ show res
                            Nothing -> return ()
                    return optmod


