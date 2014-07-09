{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Codegen where

import           Data.Function                      (on)
import           Data.List                          (sortBy)
import           Data.Map                           as Map
import           Data.String
import           Data.Word

import           Control.Applicative
import           Control.Lens                       hiding (ix)
import           Control.Monad.State

import           LLVM.General.AST
import           LLVM.General.AST.AddrSpace
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.Constant          as C
import           LLVM.General.AST.Global

newtype LLVM a = LLVM { unLLVM :: State Module a }
    deriving (Functor, Applicative, Monad, MonadState Module)

runLLVM :: Module -> LLVM a -> Module
runLLVM = flip (execState . unLLVM)

emptyModule :: String -> Module
emptyModule label = defaultModule { moduleName = label }

addDef :: Definition -> LLVM ()
addDef d = do
    defs <- gets moduleDefinitions
    modify $ \s -> s { moduleDefinitions = defs ++ [d] }

define :: Type -> String -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
define retty  label argtys body = addDef $
    GlobalDefinition $ functionDefaults
        { name = Name label
        , parameters = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
        , returnType = retty
        , basicBlocks = body
        }

external :: Type -> String -> [(Type, Name)] -> LLVM ()
external retty  label argtys = addDef $
    GlobalDefinition $ functionDefaults
        { name = Name label
        , parameters = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
        , returnType = retty
        , basicBlocks = []
        }

double :: Type
double = FloatingPointType 64 IEEE

doublePtr :: Type
doublePtr = PointerType double (AddrSpace 0)

type Names = Map String Int

uniqueName :: String -> Names -> (String, Names)
uniqueName nm ns = case Map.lookup nm ns of
    Nothing -> (nm, insert nm 1 ns)
    Just ix -> (nm ++ show ix, insert nm (ix + 1) ns)

instance IsString Name where
    fromString = Name . fromString

type SymbolTable = [(String, Operand)]

data CodegenState = CodegenState
    { _currentBlock :: Name
    , _blocks       :: Map Name BlockState
    , _symtab       :: SymbolTable
    , _blockCount   :: Int
    , _count        :: Word
    , _names        :: Names
    } deriving Show

data BlockState = BlockState
    { _idx   :: Int
    , _stack :: [Named Instruction]
    , _term  :: Maybe (Named Terminator)
    } deriving Show

makeLenses ''CodegenState
makeLenses ''BlockState

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
    deriving (Functor, Applicative, Monad, MonadState CodegenState)

emptyCodegen :: CodegenState
emptyCodegen = CodegenState (Name entryBlockName) Map.empty [] 1 0 Map.empty

execCodegen :: Codegen a -> CodegenState
execCodegen m = execState (runCodegen m) emptyCodegen

sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (_idx . snd))

createBlocks :: CodegenState -> [BasicBlock]
createBlocks m = Prelude.map makeBlock $ sortBlocks $ Map.toList (_blocks m)

makeBlock :: (Name, BlockState) -> BasicBlock
makeBlock (l, BlockState _ s t) = BasicBlock l s (maketerm t)
  where
    maketerm (Just x) = x
    maketerm Nothing = error $ "Block has no terminator: " ++ show l

entryBlockName :: String
entryBlockName = "entry"

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

addBlock :: String -> Codegen Name
addBlock bname = do
    bls <- use blocks
    ix <- use blockCount
    nms <- use names

    let new = emptyBlock ix
        (qname, supply) = uniqueName bname nms

    modify $ \s -> s
        { _blocks = insert (Name qname) new bls
        , _blockCount = ix + 1
        , _names = supply
        }
    return (Name qname)

setBlock :: Name -> Codegen Name
setBlock bname = do
    currentBlock .= bname
    return bname

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
    active <- use currentBlock
    blocks %= insert active new

current :: Codegen BlockState
current = do
    c <- use currentBlock
    blks <- use blocks
    case Map.lookup c blks of
        Just x -> return x
        Nothing -> error $ "No such block : " ++ show c

fresh :: Codegen Word
fresh = do
    i <- use count
    let j = i + 1
    count .= j
    return j

local :: Name -> Operand
local = LocalReference double

externf :: Name -> Operand
externf = ConstantOperand . C.GlobalReference double

assign :: String -> Operand -> Codegen ()
assign var x = do
    lcls <- use symtab
    symtab .= (var, x) : lcls

getVar :: String -> Codegen Operand
getVar var = do
    syms <- use symtab
    case Prelude.lookup var syms of
        Just x -> return x
        Nothing -> error $ "Local variable not in scope: " ++ show var

instr :: Instruction -> Codegen Operand
instr ins = do
    n <- fresh
    let ref = UnName n
    c <- use currentBlock
    blocks.at c._Just.stack %= \i -> i ++ [ref := ins]
    return $ local ref

terminator :: Named Terminator -> Codegen (Named Terminator)
terminator trm = do
    c <- use currentBlock
    blocks.at c._Just.term <?= trm

fadd :: Operand -> Operand -> Codegen Operand
fadd a b = instr $ FAdd NoFastMathFlags a b []

fsub :: Operand -> Operand -> Codegen Operand
fsub a b = instr $ FSub NoFastMathFlags a b []

fmul :: Operand -> Operand -> Codegen Operand
fmul a b = instr $ FMul NoFastMathFlags a b []

fdiv :: Operand -> Operand -> Codegen Operand
fdiv a b = instr $ FDiv NoFastMathFlags a b []

br :: Name -> Codegen (Named Terminator)
br val = terminator $ Do $ Br val []

cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []

call :: Operand -> [Operand] -> Codegen Operand
call fn args = instr $ Call False CC.C [] (Right fn) (toArgs args) [] []
  where toArgs = Prelude.map (\x -> (x, []))

alloca :: Type -> Codegen Operand
alloca ty = instr $ Alloca ty Nothing 0 []

store :: Operand -> Operand -> Codegen Operand
store ptr val = instr $ Store False ptr val Nothing 0 []

load :: Operand -> Codegen Operand
load ptr = instr $ Load False ptr Nothing 0 []
