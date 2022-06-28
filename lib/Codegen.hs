{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Codegen where

import Control.Applicative
import Control.Monad.RWS
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Foldable
import Data.Functor
import Data.List
import qualified Data.Map as Map
import GHC.Natural
import RegAlloc
import Repr

newtype LabelGenerator = LabelGen {labelGenCurrent :: Natural}

newLabelGen :: LabelGenerator
newLabelGen = LabelGen 0

data Assembly = AsmLabel String | Mov DataSink DataSource | Ret | Add DataSink DataSource | AsmDirective Directive

newtype Directive = Extern String

instruction :: [String] -> String
instruction [] = []
instruction (x : xs) = "  " ++ x ++ " " ++ intercalate ", " xs

instance Show Assembly where
  show (AsmLabel l) = l ++ ":"
  show (Mov from to) = instruction ["mov", show from, show to]
  show (Codegen.Add to from) = instruction ["add", show to, show from]
  show Ret = instruction ["ret"]
  show (AsmDirective dir) = show dir

instance Show Directive where
  show (Extern name) = unwords ["extern", name]

data DataSink = SinkRegister Register | SinkMemory Register Int

showMem :: (Register, Int) -> String
showMem (reg, 0) = "qword " ++ '[' : show reg ++ "]"
showMem (reg, x) = "qword " ++ '[' : show reg ++ ", " ++ show x ++ "]"

instance Show DataSink where
  show (SinkRegister reg) = show reg
  show (SinkMemory reg offt) = showMem (reg, offt)

data DataSource = SourceConstant Int | SourceRegister Register | SourceMemory Register Int

instance Show DataSource where
  show (SourceConstant nat) = show nat
  show (SourceRegister reg) = show reg
  show (SourceMemory reg offt) = showMem (reg, offt)

labelGenNext :: MonadState LabelGenerator m => m Assembly
labelGenNext = do
  LabelGen current <- get
  put $ LabelGen $ current + 1
  pure $ AsmLabel $ ".L" ++ show current

tellSingle :: MonadWriter [a] m => a -> m ()
tellSingle = tell . (: [])

asmOp :: MonadWriter [Assembly] m => MonadReader (BindingMap Register) m => Op -> m ()
asmOp (Return _) = tell [Ret]
asmOp (Assign bindings value) = case value of
  Constants ctants -> do
    regs <- traverse (asks . flip (Map.!)) bindings
    tell $ zipWith Mov (map SinkRegister regs) (map SourceConstant ctants)
  Repr.Add a b -> do
    let [target] = bindings
    targetSink <- bindingToSink target
    regA <- bindingReg a
    sourceB <- srcSource b
    -- mov <target>, <a>
    -- add <target>, <b>
    tell
      [ Mov targetSink (SourceRegister regA),
        Codegen.Add targetSink sourceB
      ]

bindingReg :: MonadReader (BindingMap Register) m => Binding -> m Register
bindingReg b = asks (Map.! b)

bindingToSink :: MonadReader (BindingMap Register) m => Binding -> m DataSink
bindingToSink b = asks (Map.! b) <&> SinkRegister

srcSource :: MonadReader (BindingMap Register) m => Source -> m DataSource
srcSource (SrcConstant c) = pure $ SourceConstant c
srcSource (SrcBinding b) = asks (Map.! b) <&> SourceRegister

mkLabel :: MonadState LabelGenerator m => Label -> m Assembly
mkLabel (Label Export name) = pure $ AsmLabel name
-- TODO : have a map to jump labels (bindings) and add to it here
mkLabel (Label Bounded _) = labelGenNext

-- TODO: sort the blocks in dependency order so that the most constrained are processed first
-- and hints are given to the leaves:
--  - grabs collisions between bindings of each block
--  - grabs calls to other blocks (args/returns)
--  - assigns forced ABI allocations for their blocks, emitting the block index and argument index
--  - resolves minor allocations (noncolliding, collisions without any special bindings)
--  - assigns Argument/Return allocation hints
--  - resolves 'call collisions' for each argument/return:
--    - grabs intersection of available non-repeating registers from each block
--    - if intersection is null, then grab the next least-used register for the blocks
--    - assign that register to that argument/return, registering it in the regspecs of each block

asmBlock ::
  MonadState LabelGenerator m =>
  MonadReader (Registry Abi) m =>
  MonadWriter [Assembly] m =>
  Block ->
  m ()
asmBlock block = do
  allocs <- mkAllocs block
  -- if it's an exported block, add a `.global` directive
  when (isExport block) $ tellSingle (AsmDirective $ Extern $ labelName $ blockLabel block)
  mkLabel (blockLabel block) >>= tell . (: [])
  runReaderT (traverse_ asmOp $ blockOps block) allocs

assembleProgram :: MonadReader (Registry Abi) m => [Block] -> m [Assembly]
assembleProgram blocks = execWriterT (evalStateT (traverse_ asmBlock blocks) newLabelGen)
