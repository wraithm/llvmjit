module Main where

import           LLVM.General.AST

import           Codegen
import           Emit
import           LambdaLift
import           Syntax           as S
import           TypeInference

process :: Program -> IO Module
process = codegen (emptyModule "sweet sweet llvm")

jitProcess :: Program -> IO Module
jitProcess = jitCodegen (emptyModule "sweet sweet llvm")

true :: Expr S.Name
true = Lam ["x", "y"] (Var "x")

trueProg :: Program
trueProg = lambdaLift [ S.Function "True" [] true ]
{-
trueProg :: Program
trueProg = lambdaLift
    [ S.Function "appTrue" [] $ App (Var "True") (Num 3)
    , S.Function "True" [] true ]
-}

main :: IO ()
main = do
    print trueProg
    _ <- process trueProg
    _ <- jitProcess trueProg

    execTypeInference true
