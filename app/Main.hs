{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (void)
import Data.Bifunctor (first, second)
-- This version of groupBy garantees that the inner lists aren't empty
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import Text.Pandoc.Readers (Reader, readers)
import Text.Pandoc.Class (PandocIO, PandocMonad, runIO)

import Lib

getPandocReader :: PandocMonad m => Text -> Maybe (Reader m)
getPandocReader k = listToMaybe $ fmap snd $ filter ((== k) . fst) readers


data CodeType = Lit | Code
  deriving (Show, Eq)

type Chunk = (CodeType, Text)

sameCodeType :: Chunk -> Chunk -> Bool
sameCodeType a b = fst a == fst b

separateText :: Text -> [Chunk]
separateText t = fmap flatten $ NE.groupBy sameCodeType
               $ fmap separateText' $ T.lines t
  where
    separateText' :: Text -> Chunk
    separateText' s
      | "///" `T.isPrefixOf` s = (Lit , T.drop 3 s)
      | otherwise              = (Code, s)
    flatten :: NonEmpty Chunk -> Chunk
    flatten chunks = let tp = fst $ NE.head chunks
                     in  (tp, T.unlines $ fmap snd $ NE.toList chunks)
    


singleArgument :: [String] -> String
singleArgument [arg] = arg
singleArgument _ = error "Please supply one single argument (the file)"

-- main = print $ fmap fst (readers :: [(Text, Reader PandocIO)])
main :: IO ()
main = do
  arg <- fmap singleArgument getArgs
  text <- TIO.readFile arg
  void $ sequence $ fmap print $ separateText $ text



