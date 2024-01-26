module Main (main) where

import Control.Monad (void)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import Text.Pandoc.Readers (Reader, readers)
import Text.Pandoc.Class (PandocIO, PandocMonad, runIO)

import Chunks (separateText)

getPandocReader :: PandocMonad m => Text -> Maybe (Reader m)
getPandocReader k = listToMaybe $ fmap snd $ filter ((== k) . fst) readers

    


singleArgument :: [String] -> String
singleArgument [arg] = arg
singleArgument _ = error "Please supply one single argument (the file)"

-- main = print $ fmap fst (readers :: [(Text, Reader PandocIO)])
main :: IO ()
main = do
  arg <- fmap singleArgument getArgs
  text <- TIO.readFile arg
  let chunks = separateText $ text
  void $ sequence $ fmap (TIO.putStrLn . snd) chunks



