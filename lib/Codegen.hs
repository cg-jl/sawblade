{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Codegen where

import Control.Monad.RWS
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Foldable
import Data.List
import qualified Data.Map as Map
import GHC.Natural
import RegAlloc
import Repr

newtype LabelGenerator = LabelGen {labelGenCurrent :: Natural}

newLabelGen :: LabelGenerator
newLabelGen = LabelGen 0

data Assembly = AsmLabel String | Mov DataSink DataSource | Ret

instruction :: [String] -> String
instruction [] = []
instruction (x : xs) = "  " ++ x ++ " " ++ intercalate ", " xs

instance Show Assembly where
  show (AsmLabel l) = l ++ ":"
  show (Mov from to) = instruction ["mov", show from, show to]
  show Ret = instruction ["ret"]

data DataSink = SinkRegister Register | SinkMemory Register Int

showMem :: (Register, Int) -> String
showMem (reg, 0) = "qword " ++ '[' : show reg ++ "]"
showMem (reg, x) = "qword " ++ '[' : show reg ++ ", " ++ show x ++ "]"

instance Show DataSink where
  show (SinkRegister reg) = show reg
  show (SinkMemory reg offt) = showMem (reg, offt)

data DataSource = SourceConstant Int | SourceRegister Register | SourceMemory Register Int

instance Show DataSource where
  show (SourceConstant nat) = '#' : show nat
  show (SourceRegister reg) = show reg
  show (SourceMemory reg offt) = showMem (reg, offt)

labelGenNext :: MonadState LabelGenerator m => m Assembly
labelGenNext = do
  LabelGen current <- get
  put $ LabelGen $ current + 1
  pure $ AsmLabel $ ".L" ++ show current

asmOp :: MonadWriter [Assembly] m => MonadReader (BindingMap Register) m => Op -> m ()
asmOp (Return _) = tell [Ret]
asmOp (Assign bindings value) = case value of
  Constants ctants -> do
    regs <- traverse (asks . flip (Map.!)) bindings
    tell $ zipWith Mov (map SinkRegister regs) (map SourceConstant ctants)

mkLabel :: MonadState LabelGenerator m => Label -> m Assembly
mkLabel (Label Export name) = pure $ AsmLabel name
-- TODO : have a map to jump labels (bindings) and add to it here
mkLabel (Label Bounded _) = labelGenNext

asmBlock :: MonadState LabelGenerator m => MonadReader (Registry Abi) m => MonadWriter [Assembly] m => Block -> m ()
asmBlock block = do
  allocs <- mkAllocs block
  mkLabel (blockLabel block) >>= tell . (: [])
  runReaderT (traverse_ asmOp $ blockOps block) allocs

assembleProgram :: MonadReader (Registry Abi) m => [Block] -> m [Assembly]
assembleProgram blocks = execWriterT (evalStateT (traverse_ asmBlock blocks) newLabelGen)
