{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Parser (parse) where

import Control.Monad (void, when)
import Control.Monad.State (MonadState, MonadTrans (lift))
import Control.Monad.Trans ()
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.Char (isPrint, isSpace)
import Data.Foldable (find)
import Data.Functor ((<&>))
import Data.Maybe (isJust)
import Repr
  ( Abi (abiReturns),
    Binding (..),
    Block (Block),
    Label (Label),
    Op (..),
    Ref,
    Register,
    Registry,
    Value (..),
    Visibility (Export, Bounded),
    addToRegistry,
    emptyAbi,
  )
import Text.Parsec
  ( ParseError,
    ParsecT,
    SourceName,
    Stream,
    anyChar,
    between,
    char,
    digit,
    label,
    many1,
    manyTill,
    optionMaybe,
    optional,
    runPT,
    satisfy,
    sepBy,
    sepEndBy,
    sepEndBy1,
    space,
    string,
    (<?>),
    (<|>),
  )

parse :: MonadState (Registry Abi) m => SourceName -> String -> m (Either ParseError Block)
parse = runPT block ()

sepChars :: String
sepChars = "()[]{};\""

isBinding :: Char -> Bool
isBinding c = c `notElem` sepChars && not (isSpace c) && isPrint c

bindingName :: Stream s m Char => ParsecT s u m String
bindingName = label (many1 $ satisfy isBinding) "valid binding characters"

whitespace :: Stream s m Char => ParsecT s u m ()
whitespace = (space >> optional whitespace) <|> (string "//" >> lineComment >> optional whitespace) <?> "whitespace"
  where
    lineComment = void $ manyTill (char '\n') anyChar

binding :: Stream s m Char => ParsecT s u m Binding
binding = char '%' >> bindingName <&> Binding <?> "binding"

semicolon :: Stream s m Char => ParsecT s u m ()
semicolon = label (void $ char ';') "semicolon"

blockLabel :: Stream s m Char => ParsecT s u m Label
blockLabel = label (exportLabel <|> boundLabel) "block label"
  where
    exportLabel = between (char '"') (char '"') bindingName <&> Label Export <?> "export label"
    boundLabel = binding <&> Label Bounded . unBinding <?> "bound label"

body :: Stream s m Char => ParsecT s u m a -> ParsecT s u m [a]
body inner = do
  char '{' <?> "'{' to start a body"
  optional whitespace
  values <- inner `sepEndBy` (semicolon >> optional whitespace)
  char '}' <?> "'}' to end the started body"
  pure values

register :: Stream s m Char => ParsecT s u m Register
register = do
  name <- bindingName
  case find (\x -> show x == name) [minBound .. maxBound] of
    Just reg -> pure reg
    Nothing -> fail $ "unknown register: " ++ show name

registerList :: Stream s m Char => ParsecT s u m [Register]
registerList = do
  char '[' <?> "'[' to start a register list"
  optional whitespace
  regs <- register `sepEndBy` whitespace
  char ']' <?> "']' to end the register list"
  pure regs

newtype AbiKey = AbiReturns [Register]

addAbiKey :: Abi -> AbiKey -> Abi
addAbiKey abi = \case
  AbiReturns regs -> abi {abiReturns = regs}

buildAbi :: [AbiKey] -> Abi
buildAbi = foldl addAbiKey emptyAbi

abiKey :: Stream s m Char => ParsecT s u m AbiKey
abiKey = returns
  where
    returns = do
      string "return"
      optional whitespace
      registerList <&> AbiReturns

abi :: (Stream s m Char, MonadState (Registry Abi) m) => ParsecT s u m (Ref Abi)
abi = (body abiKey >>= addToRegistry . buildAbi) <?> "abi spec"

-- TODO: calls and other expressions should have 'primitives': bindings or constants

value :: Stream s m Char => ParsecT s u m Value
value = constants
  where
    constants = (many1 digit <&> read) `sepEndBy1` whitespace <&> Constants

op :: Stream s m Char => ParsecT s u m Op
op = do
  bindings <- binding `sepEndBy` whitespace
  when (null bindings) $ fail "empty binding list"
  optional whitespace
  gotEquals <- optionMaybe (char '=' >> optional whitespace) <&> isJust
  if gotEquals
    then Assign bindings <$> (optional whitespace >> value)
    else pure $ Return bindings

block :: (Stream s m Char, MonadState (Registry Abi) m) => ParsecT s u m Block
block = do
  string "block" <?> "keyword 'block'"
  optional whitespace
  name <- Parser.blockLabel
  optional whitespace
  -- TODO: block argument list
  opt_abi <- runMaybeT $ MaybeT (optionMaybe (string "::" >> optional whitespace)) >> lift abi
  optional whitespace
  ops <- body op
  pure $ Block name opt_abi ops
