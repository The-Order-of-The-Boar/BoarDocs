{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import Text.Megaparsec (runParserT, errorBundlePretty)
import Control.Monad.Trans.State (evalState)

import FParser (defaultFState, parseFile)

singleArgument :: [String] -> String
singleArgument [arg] = arg
singleArgument _ = error "Please supply one single argument (the file)"

main :: IO ()
main = do
  arg <- fmap singleArgument getArgs
  text <- TIO.readFile arg
  print text
  case (flip evalState) defaultFState $ runParserT parseFile arg text of
    Right x -> print x
    Left e -> putStrLn $ errorBundlePretty e



