{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Prelude hiding (many, some)

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
import Text.Megaparsec (ParsecT, runParserT, (<?>), many, some, errorBundlePretty)
import qualified Text.Megaparsec.Char as MPC
import Control.Monad.Trans.State (State, evalState)
import Data.Void (Void)

type TxtReader = ReaderOptions -> Text -> PandocIO Pandoc

-- Gets the reader associated with a name (Ex: "markdown")
getPandocTxtReader :: Text -> Maybe TxtReader
getPandocTxtReader k =
  case listToMaybe $ fmap snd $ filter ((== k) . fst) readers of
    Just (TextReader r) -> Just r
    -- Ignores bytestring readers
    _ -> Nothing

data BoarConfig = BoarConfig { markup :: Maybe (Text, TxtReader)
                             }
defaultBoarConfig :: BoarConfig
defaultBoarConfig = BoarConfig { markup = Nothing
                               }



setMarkupPrefix :: Text
setMarkupPrefix = "boar set markup "




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

data FState = FState { bCfg :: BoarConfig
                     }
defaultFState :: FState
defaultFState = FState { bCfg = defaultBoarConfig
                       }

type FParser = ParsecT Void Text (State FState)

parseFile :: FParser ()
parseFile =
  do
    _ <- MPC.char 'x'
    return ()
  <?> "x"


