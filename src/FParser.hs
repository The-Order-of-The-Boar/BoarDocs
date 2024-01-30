module FParser ( FState
               , defaultFState
               , FParser
               , parseFile
               ) where

import Control.Monad.Trans.State (State)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

import BoarConfig (BoarConfig, defaultBoarConfig)

-- The state the parser maintains and mutates while parsing
data FState = FState { bCfg :: BoarConfig
                     }
defaultFState :: FState
defaultFState = FState { bCfg = defaultBoarConfig
                       }

-- Parser for the files, maintains a state inside
type FParser = ParsecT Void Text (State FState)

-- Parses the input file
-- Currently just a nonsense match
parseFile :: FParser ()
parseFile =
  do
    _ <- char 'x'
    return ()
  <?> "x"

