{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Repr where

import Control.Monad.State.Class
import Data.Functor ((<&>))
import Data.List (elemIndex)
import Data.Maybe
import Data.String
import GHC.Natural (Natural)

-- typed index
newtype Ref a = Ref {unRef :: Natural}
  deriving (Eq)

data Registry a = Registry {unRegistry :: [a], registryLen :: Natural}

instance Show a => Show (Registry a) where
  show = show . unRegistry

emptyRegistry :: Registry a
emptyRegistry = Registry [] 0

addToRegistry :: MonadState (Registry a) m => a -> m (Ref a)
addToRegistry elem = do
  Registry currents len <- get
  put $ Registry (elem : currents) (len + 1)
  pure $ Ref len

registryIndex :: Ref a -> Registry a -> Maybe a
registryIndex (Ref i) (Registry r _) = indexMaybe r i

instance Show (Ref a) where
  show (Ref i) = '#' : show i

data Visibility = Export | Bounded
  deriving (Eq)

instance Show Visibility where
  show Export = "export"
  show Bounded = "bounded"

data Label = Label {labelVisibility :: Visibility, labelName :: String}
  deriving (Eq)

instance Show Label where
  show (Label Bounded name) = '@' : show name
  show (Label Export name) = show name

data Op = Return [Binding] | Assign [Binding] Value
  deriving (Eq)

instance Show Op where
  show (Return bindings) = unwords $ map show bindings
  show (Assign bindings value) = unwords $ map show bindings ++ ["=", show value]

newtype Binding = Binding {unBinding :: String}
  deriving (Eq, Ord)

instance IsString Binding where
  fromString = Binding

instance Show Binding where
  show = ('%' :) . unBinding

newtype Abi = Abi {abiReturns :: [Register]}
  deriving (Show, Eq)

emptyAbi :: Abi
emptyAbi = Abi []

-- abi is behind a ref because there will be a way to declare an ABI separately.
--
data Block = Block {blockLabel :: Label, blockAbi :: Maybe (Ref Abi), blockArgs :: [Binding], blockOps :: [Op]}
  deriving (Eq)

isExport :: Block -> Bool
isExport = isExportLabel . blockLabel
  where
    isExportLabel (Label Export _) = True
    isExportLabel _ = False

instance Show Block where
  show (Block label abi args ops) = "block " ++ show label ++ maybe "" (\abi -> " :: " ++ show abi ++ " ") abi ++ argsList ++ opsList
    where
      opsList
        | null ops = "{}"
        | otherwise = "{\n" ++ unlines (map show ops) ++ "\n}"

      argsList
        | null args = "()"
        | otherwise = '(' : unwords (map show args) ++ ")"

indexMaybe :: [a] -> Natural -> Maybe a
indexMaybe [] = const Nothing
indexMaybe (x : xs) = \case
  0 -> Just x
  n -> indexMaybe xs (n - 1)

refMaybe :: Eq a => a -> [a] -> Maybe (Ref a)
refMaybe x xs = elemIndex x xs <&> Ref . fromIntegral

-- TODO: extend with more stuff
data Value = Constants [Int] | Add Binding Source
  deriving (Eq)

data Source = SrcBinding Binding | SrcConstant Int
  deriving (Eq)

instance Show Source where
  show (SrcBinding b) = show b
  show (SrcConstant c) = show c

instance Show Value where
  show (Constants values) = unwords $ map show values
  show (Add a b) = unwords ["add", show a, show b]

data Register
  = Rsp
  | Rbp
  | Rax
  | Rbx
  | Rcx
  | Rdx
  | Rsi
  | Rdi
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15
  deriving (Enum, Bounded, Eq)

instance Show Register where
  show Rsp = "rsp"
  show Rbp = "rbp"
  show Rax = "rax"
  show Rbx = "rbx"
  show Rcx = "rcx"
  show Rdx = "rdx"
  show Rsi = "rsi"
  show Rdi = "rdi"
  show R9 = "r9"
  show R10 = "r10"
  show R11 = "r11"
  show R12 = "r12"
  show R13 = "r13"
  show R14 = "r14"
  show R15 = "r15"
