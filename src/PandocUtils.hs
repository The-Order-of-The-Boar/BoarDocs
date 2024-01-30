module PandocUtils ( TxtReader
                   , getPandocTxtReader
                   ) where

import Data.Maybe (mapMaybe, listToMaybe)
import Data.Text (Text)
import Text.Pandoc.Class (PandocIO, PandocMonad, runIOorExplode)
import Text.Pandoc.Definition (Pandoc)
import Text.Pandoc.Options (ReaderOptions, def)
import Text.Pandoc.Readers (Reader (TextReader), readers)

type TxtReader = ReaderOptions -> Text -> PandocIO Pandoc

-- Gets the reader associated with a name (Ex: "markdown")
getPandocTxtReader :: Text -> Maybe TxtReader
getPandocTxtReader k = listToMaybe       -- Similar to head, but Maybe
                     $ mapMaybe onlyText -- Ignores bytestring readers
                     $ fmap snd          -- Ignores the reader name
                     $ filter ((== k) . fst) readers
  where
    -- Ignores the bytestring readers
    onlyText :: Reader PandocIO -> Maybe TxtReader
    onlyText (TextReader r) = Just r
    onlyText _ = Nothing

