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

-- A single line, either Literature (markup), Config (metadata) or Code (the
-- code itself)
parseLine :: FParser ()
parseLine = do
  r <- parseLiterature <|> parseConfig <|> parseCodeLine
  void eol
  return r

-- Parses a line of literature/markup
parseLiterature :: FParser ()
parseLiterature = do
  _ <- string "////"
  void tillLineEnd

-- Parses a line of code
parseCodeLine :: FParser ()
parseCodeLine = void $ tillLineEnd

-- Parses a config line ("///!")
parseConfig :: FParser ()
parseConfig = do
  _ <- string "///!"
  -- tries to parse a markup definition or throws an error while skipping the
  -- line
  parseSetMarkup <|> tillLineEndFail "valid config"

-- Modifies the configuration, setting the markup
-- In case of invalid markup, it reports an error but doesn't fail
parseSetMarkup :: FParser ()
parseSetMarkup = do
  -- default prefix for markup definition
  _ <- string "boar set markup "
  -- no trimming is being done
  m <- tillLineEnd
  case getPandocTxtReader m of
    Just r -> do
      -- modify the configuration, setting the new markup
      lift $ modify (\s -> s { bCfg = (bCfg s) { markup = Just (m, r) } })
    Nothing -> do
      -- fails if the markup wasn't found
      betterFailureReg (T.unpack m) "valid markup"


-- Returns the rest of the line as a string, but not consuming the eol
-- Usually you will prefer to use tillLineEnd >>= eol
tillLineEnd :: FParser Text
tillLineEnd = do
  s <- takeWhileP Nothing (\x -> not $ x `elem` ("\r\n" :: String))
  return s

-- Fails showing the rest of the line as unexpected input and the String
-- argument as the label of the expected one
tillLineEndFail :: String -> FParser ()
tillLineEndFail s = do
  rest <- tillLineEnd
  betterFailureReg (T.unpack rest) s

-- Wrapper to make registerFailure easier to use, by restricting the types
-- The first string is the unexpected input, second is a label describing what
-- was expected
-- Any of them can be an empty string
betterFailureReg :: String -> String -> FParser ()
betterFailureReg u e = registerFailure un ex
  where
    un = fmap Tokens $ NE.nonEmpty u
    ex = case NE.nonEmpty e of
      Just ne -> Set.singleton (Label ne)
      Nothing -> Set.empty


