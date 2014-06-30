module Main where

import           LLVM.General.AST

import           Codegen
import           Emit
import           JIT
import           LambdaLift
import           Syntax           as S

process :: Program -> IO Module
process = codegen (emptyModule "sweet sweet llvm")

jitProcess :: Program -> IO Module
jitProcess = jitCodegen (emptyModule "sweet sweet llvm")

true = lambdaLift [ S.Function "true" [] $ Lam ["x", "y"] (Var "x") ]

main :: IO ()
main = print true >> process true >> jitProcess true >> return ()
