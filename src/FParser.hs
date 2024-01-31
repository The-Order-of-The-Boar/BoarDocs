{-# LANGUAGE OverloadedStrings #-}

module FParser ( FState
               , defaultFState
               , FParser
               , parseFile
               ) where

import Control.Monad (void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (State, modify)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

import BoarConfig (BoarConfig (..), defaultBoarConfig)
import PandocUtils (getPandocTxtReader)

-- The state the parser maintains and mutates while parsing
data FState = FState { bCfg :: BoarConfig
                     } deriving Show
defaultFState :: FState
defaultFState = FState { bCfg = defaultBoarConfig
                       }

-- Parser for the files, maintains a state inside
type FParser = ParsecT Void Text (State FState)

-- Parses the input file
parseFile :: FParser ()
parseFile = void $ many parseLine

parseLine :: FParser ()
parseLine = do
  r <- parseLiterature <|> parseConfig <|> parseCodeLine
  void eol
  return r

parseLiterature :: FParser ()
parseLiterature = do
  _ <- string "////"
  void tillLineEnd

parseCodeLine :: FParser ()
parseCodeLine = void $ tillLineEnd

parseConfig :: FParser ()
parseConfig = do
  _ <- string "///!"
  parseSetMarkup <|> tillLineEndFail "valid config"

parseSetMarkup :: FParser ()
parseSetMarkup = do
  _ <- string "boar set markup "
  m <- tillLineEnd
  case getPandocTxtReader m of
    Just r -> do
      lift $ modify (\s -> s { bCfg = (bCfg s) { markup = Just (m, r) } })
    Nothing -> do
      betterFailureReg (T.unpack m) "valid markup"


tillLineEnd :: FParser Text
tillLineEnd = do
  s <- takeWhileP Nothing (\x -> not $ x `elem` ("\r\n" :: String))
  return s

tillLineEndFail :: String -> FParser ()
tillLineEndFail s = do
  rest <- tillLineEnd
  betterFailureReg (T.unpack rest) s

betterFailureReg :: String -> String -> FParser ()
betterFailureReg u e = registerFailure un ex
  where
    un = fmap Tokens $ NE.nonEmpty u
    ex = case NE.nonEmpty e of
      Just ne -> Set.singleton (Label ne)
      Nothing -> Set.empty


