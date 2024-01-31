module Main (main) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import Text.Megaparsec (runParserT, errorBundlePretty)
import Control.Monad.Trans.State (runState)

import FParser (FState (..), defaultFState, parseFile)

singleArgument :: [String] -> String
singleArgument [arg] = arg
singleArgument _ = error "Please supply one single argument (the file)"

main :: IO ()
main = do
  arg <- fmap singleArgument getArgs
  text <- TIO.readFile arg
  print text
  let (r, s) = runState (runParserT parseFile arg text) defaultFState
  case r of
    Right x -> print x
    Left e -> putStrLn $ errorBundlePretty e

  print s

  TIO.putStrLn $ T.unlines $ literature s


