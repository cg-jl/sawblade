module Main where

import Control.Monad.State
import Data.Functor
import Data.Semigroup ((<>))
import Options.Applicative
import Parser
import Codegen
import Repr
import System.IO
import qualified Text.Parsec as Parsec

data InputKind
  = FileInput FilePath
  | StdInput

getInput :: InputKind -> IO String
getInput (FileInput "-") = getContents
getInput (FileInput file) = readFile file
getInput StdInput = getContents

getName :: InputKind -> String
getName (FileInput "-") = "<stdin>"
getName (FileInput file) = file
getName StdInput = "<stdin>"

parse' :: InputKind -> IO (Either Parsec.ParseError Block, Registry Abi)
parse' input = do
  let name = getName input
  source <- getInput input
  pure $ runState (parse name source) emptyRegistry

processParsed :: (Either Parsec.ParseError Block, Registry Abi) -> IO ()
processParsed (Left error, _) = hPrint stderr error
processParsed (Right block, abis) = do
  let result = assembleProgram [block] abis
  putStrLn $ unlines $ map show result

inputKind :: Parser InputKind
inputKind = file <|> pure StdInput
  where
    file = strOption (long "file" <> short 'f' <> metavar "FILE" <> help "optional filename to use instead of stdin" <> value "-" <> showDefault) <&> FileInput

main :: IO ()
main = processParsed =<< parse' =<< execParser opts
  where
    opts = info (inputKind <**> helper) (fullDesc <> header "abism - a compiler backend")
