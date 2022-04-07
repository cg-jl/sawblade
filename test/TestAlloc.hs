{-# LANGUAGE OverloadedStrings #-}

module TestAlloc (testAlloc) where

import Control.Monad.State
import Data.Functor
import Data.Map ((\\))
import RegAlloc
import Repr
import Test.HUnit

sampleAbi :: Abi
sampleAbi = Abi {abiReturns = [Rsi, Rdx]}

sampleRegistry :: Registry Abi
sampleRegistry = execState (addToRegistry sampleAbi) emptyRegistry

sampleBlock :: Block
sampleBlock = Block (Repr.Label Export "main") (Just (Ref 0)) [Assign ["hello", "world"] $ Constants [1, 2], Return ["hello", "world"]]

ensureNoLeftOuts :: Test
ensureNoLeftOuts = null (snd $ allocBlock sampleBlock sampleRegistry) ~? "must have no left outs from allocations"

-- XXX: How do I test that abi stuff is correctly allocated?
--      How do I test that clobbers are respected?
abiPreAllocs :: Test
abiPreAllocs = do
  let preAllocs = mkAllocsFromAbi sampleAbi (blockOps sampleBlock)
      finalAllocs = mkAllocs sampleBlock sampleRegistry
      diff = finalAllocs \\ preAllocs

  "abi must be respected at all costs" ~: preAllocs @=? finalAllocs

-- TODO: clobbers from abi
-- ensure no left outs
testAlloc :: Test
testAlloc = test ["no left outs" ~: ensureNoLeftOuts, "complete abi pre-allocs" ~: abiPreAllocs]
