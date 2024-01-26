{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (void)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import Text.Pandoc.Readers (Reader, readers)
import Text.Pandoc.Class (PandocIO, PandocMonad, runIO)

import Chunks (CodeType (Cfg), Chunk, separateText)

type PandReader = Reader PandocIO

-- MovingConfig is called this way because it keeps moving along the code. As
-- the Chunks are parsed this config gets recreated (when Cfg is found)
data MovingConfig = MovingConfig { markup :: Maybe (Text, PandReader) }

defaultMovingConfig :: MovingConfig
defaultMovingConfig = MovingConfig { markup = Nothing }

-- Gets the reader associated with a name (Ex: "markdown")
getPandocReader :: PandocMonad m => Text -> Maybe (Reader m)
getPandocReader k = listToMaybe $ fmap snd $ filter ((== k) . fst) readers



setMarkupPrefix :: Text
setMarkupPrefix = "boar set markup "

moveConfig :: MovingConfig -> Chunk -> MovingConfig
moveConfig cfg (Cfg, t)
  | setMarkupPrefix `T.isPrefixOf` t =
    let s = T.strip $ T.drop (T.length setMarkupPrefix) t
    in case getPandocReader s of
      Just reader -> cfg { markup = Just (s, reader) }
      Nothing -> error "Unknown markup"
moveConfig cfg _ = cfg

singleArgument :: [String] -> String
singleArgument [arg] = arg
singleArgument _ = error "Please supply one single argument (the file)"

main :: IO ()
main = do
  arg <- fmap singleArgument getArgs
  text <- TIO.readFile arg
  let chunks = separateText $ text
  void $ sequence $ fmap (\(x, y) -> print x >> TIO.putStrLn y) chunks

  print $ fmap fst $ markup $ foldl moveConfig defaultMovingConfig chunks




