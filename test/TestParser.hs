module TestParser (parserTests) where

--import Parser

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State
import Data.Functor
import Data.Maybe
import Parser
import Repr
import Test.HUnit
import Text.Parsec

testParserT :: Monad m => ParsecT String () m a -> String -> m (Either ParseError a)
testParserT p = runPT p () "<test>"

testParser :: Parsec String () a -> String -> Either ParseError a
testParser p = runP p () "<test>"

-- TODO: test for errors as well
testLabel :: Test
testLabel =
  test
    [ "bounded label" ~: Right (Repr.Label Bounded "bounded-label") ~=? testParser Parser.blockLabel "@bounded-label",
      "export  label" ~: Right (Repr.Label Export "export-label") ~=? testParser Parser.blockLabel "\"export-label\""
    ]

testAbiParser :: Monad m => String -> m (Either ParseError Abi)
testAbiParser source = runExceptT $
  flip evalStateT emptyRegistry $ do
    ref <- testParserT Parser.abi source >>= liftEither
    gets (registryIndex ref) <&> fromJust

testAbi :: Test
testAbi = test ["empty abi" ~: testEmptyAbi, "returns" ~: returns]
  where
    returns = Right (Abi {abiReturns = [Rax, Rdx, Rsi]}) ~=? runIdentity (testAbiParser "{ return [rax rdx rsi]; }")
    testEmptyAbi = Right emptyAbi ~=? runIdentity (testAbiParser "{}")

testOp :: Test
testOp = test ["return" ~: testReturn, "assign" ~: testAssign]
  where
    testReturn = Right (Return $ ["hello"] <&> Binding) ~=? testParser Parser.op "%hello"
    testAssign = Right (Assign (["hello", "world"] <&> Binding) (Constants [1, 0])) ~=? testParser Parser.op "%hello %world = 1 0"

testValue :: Test
testValue = test ["constants" ~: testConstants]
  where
    testConstants = Right (Constants [1 .. 5]) ~=? testParser Parser.value (unwords $ map show [1 .. 5])

testBlock :: Test
testBlock = test ["implicit abi" ~: testImplicitAbi, "explicit abi" ~: testExplicitAbi]
  where
    testBlock :: Maybe (Ref Abi) -> String -> Test
    testBlock abi source = Right (Block (Repr.Label Bounded "hello") abi []) ~=? evalState (testParserT Parser.block ("block @hello " ++ source ++ "{}")) emptyRegistry

    testImplicitAbi = testBlock Nothing ""
    testExplicitAbi = testBlock (Just $ Ref 0) ":: {}"

parserTests :: Test
parserTests = test ["label" ~: testLabel, "abi" ~: testAbi, "op" ~: testOp]
