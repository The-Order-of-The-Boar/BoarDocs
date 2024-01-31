module BoarConfig ( BoarConfig (..)
                  , defaultBoarConfig
                  ) where

import Data.Text (Text)

import PandocUtils (TxtReader)

data BoarConfig = BoarConfig { markup :: Maybe (Text, TxtReader)
                             } deriving Show
defaultBoarConfig :: BoarConfig
defaultBoarConfig = BoarConfig { markup = Nothing
                               }
