module BoarConfig ( BoarConfig
                  , defaultBoarConfig
                  ) where

import Data.Text (Text)

import PandocUtils (TxtReader)

data BoarConfig = BoarConfig { markup :: Maybe (Text, TxtReader)
                             }
defaultBoarConfig :: BoarConfig
defaultBoarConfig = BoarConfig { markup = Nothing
                               }

