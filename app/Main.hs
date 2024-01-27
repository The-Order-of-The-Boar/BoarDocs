{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (void, foldM)
import Data.Maybe (listToMaybe, fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import Text.Pandoc.Class (PandocIO, PandocMonad, runIOorExplode)
import Text.Pandoc.Definition (Pandoc)
import Text.Pandoc.Options (ReaderOptions, def)
import Text.Pandoc.Readers (Reader (TextReader), readers)

import Chunks (CodeType (..), Chunk, separateText)

type TxtReader = ReaderOptions -> Text -> PandocIO Pandoc

-- Gets the reader associated with a name (Ex: "markdown")
getPandocTxtReader :: Text -> Maybe TxtReader
getPandocTxtReader k =
  case listToMaybe $ fmap snd $ filter ((== k) . fst) readers of
    Just (TextReader r) -> Just r
    _ -> Nothing


-- MovingConfig is called this way because it keeps moving along the code. As
-- the Chunks are parsed this config gets recreated (when Cfg is found)
data MovingConfig = MovingConfig { markup :: Maybe (Text, TxtReader) }

defaultMovingConfig :: MovingConfig
defaultMovingConfig = MovingConfig { markup = Nothing }

data Accumulator = Accumulator { movingCfg :: MovingConfig
                               ,   pChunks :: [Pandoc]
                               }
defaultAccumulator :: Accumulator
defaultAccumulator = Accumulator { movingCfg = defaultMovingConfig
                                 ,   pChunks = []
                                 }


setMarkupPrefix :: Text
setMarkupPrefix = "boar set markup "

parseChunks :: [Chunk] -> IO Accumulator
parseChunks = foldM parseChunk defaultAccumulator

parseChunk :: Accumulator -> Chunk -> IO Accumulator
parseChunk acc (Cfg, s) =
  return acc { movingCfg = foldl parseCfgLine (movingCfg acc) $ T.lines s }
parseChunk acc (Lit, s) = do
  pc <- executeConfig (movingCfg acc) s
  return acc { pChunks = (pChunks acc) ++ [pc] }
parseChunk acc (Code, _) = return acc -- TODO

parseCfgLine :: MovingConfig -> Text -> MovingConfig
parseCfgLine cfg ln
  | setMarkupPrefix `T.isPrefixOf` ln =
    let name = T.drop (T.length setMarkupPrefix) ln
    in case getPandocTxtReader name of
      Just reader -> cfg { markup = Just (name, reader) }
      Nothing -> error $ "Unknown markup " ++ (T.unpack name)
  | otherwise = error $ "unknown cfg " ++ (T.unpack ln)

executeConfig :: MovingConfig -> Text -> IO Pandoc
executeConfig cfg t = runIOorExplode $ (snd $ fromJust $ markup cfg) def t



singleArgument :: [String] -> String
singleArgument [arg] = arg
singleArgument _ = error "Please supply one single argument (the file)"

main :: IO ()
main = do
  arg <- fmap singleArgument getArgs
  text <- TIO.readFile arg
  let chunks = separateText $ text
  void $ sequence $ fmap (\(x, y) -> print x >> TIO.putStrLn y) chunks

  parsedChunks <- parseChunks chunks
  print $ fmap fst $ markup $ movingCfg parsedChunks
  print $ pChunks parsedChunks




