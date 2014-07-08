module Main where

import           LLVM.General.AST

import           Codegen
import           Emit
import           LambdaLift
import           Syntax           as S

process :: Program -> IO Module
process = codegen (emptyModule "sweet sweet llvm")

jitProcess :: Program -> IO Module
jitProcess = jitCodegen (emptyModule "sweet sweet llvm")

true :: Program
true = lambdaLift 
    [ S.Function "appTrue" [] $ App (Var "True") (Num 3)
    , S.Function "True" [] $ Lam ["x", "y"] (Var "x") ]

main :: IO ()
main = print true >> process true >> jitProcess true >> return ()
