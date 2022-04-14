{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Parser where

import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Char
import Data.Foldable
import Data.Functor
import Data.Maybe
import Debug.Trace
import Repr
import Text.Parsec hiding (parse)

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
    boundLabel = char '@' >> bindingName <&> Label Bounded <?> "bounded label"

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

constant :: Stream s m Char => ParsecT s u m Int
constant = many1 digit <&> read <?> "numeric constant"

-- TODO: calls and other expressions should have 'primitives': bindings or constants

value :: Stream s m Char => ParsecT s u m Value
value = (constants <?> "list of constants") <|> (add <?> "'add' pseudo-instruction")
  where
    constants = constant `sepEndBy1` whitespace <&> Constants
    add = do
      string "add" <?> "keyword 'add'"
      whitespace
      s1 <- binding
      whitespace
      Add s1 <$> source

    source = (binding <&> SrcBinding) <|> (constant <&> SrcConstant)

op :: Stream s m Char => ParsecT s u m Op
op = do
  bindings <- binding `sepEndBy` whitespace
  when (null bindings) $ fail "empty binding list"
  optional whitespace
  gotEquals <- optionMaybe (char '=' >> optional whitespace) <&> isJust
  if gotEquals
    then Assign bindings <$> (optional whitespace >> value)
    else pure $ Return bindings

hasArgs :: Stream s m Char => ParsecT s u m Bool
hasArgs = try (optionMaybe $ lookAhead (char '(')) <&> isJust

args :: Stream s m Char => ParsecT s u m [Binding]
args = do
  char '(' <?> "left paren to start argument list"
  optional whitespace
  bindings <- binding `sepEndBy` whitespace
  char ')' <?> "right paren to end argument list"
  pure bindings

blockSpec ::
  Stream s m Char =>
  MonadState (Registry Abi) m =>
  ParsecT s u m (Maybe (Ref Abi), [Binding])
blockSpec = do
  has <- optionMaybe (string "::") <&> isJust
  if has then optional whitespace >> pBlockSpec else pure (Nothing, [])
  where
    pBlockSpec = do
      opt_abi <- optionMaybe abi
      args <- hasArgs >>= \r -> if r then args else pure []
      pure (opt_abi, args)

block ::
  Stream s m Char =>
  MonadState (Registry Abi) m =>
  ParsecT s u m Block
block = do
  string "block" <?> "keyword 'block'"
  optional whitespace
  name <- Parser.blockLabel
  optional whitespace
  (opt_abi, args) <- blockSpec
  optional whitespace
  ops <- body op
  pure $ Block name opt_abi args ops
