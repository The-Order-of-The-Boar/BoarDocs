module Main (main) where

import Data.Text (Text)
import Text.Pandoc.Readers (Reader, readers)
import Text.Pandoc.Class (PandocIO, runIO)

import Lib

main :: IO ()
main = print $ fmap fst (readers :: [(Text, Reader PandocIO)])
