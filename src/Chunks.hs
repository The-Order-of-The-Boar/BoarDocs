{-# LANGUAGE OverloadedStrings #-}

module Chunks ( CodeType (..)
              , Chunk
              , sameCodeType
              , separateText
              ) where


import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T


-- Lit (Literature)    for comments that will be rendered
-- Code                for code, that will also be rendered (in a code block)
-- Cfg (Configuration) for settings about the behaviour of Boar Docs or sets
--                     some different kind of behaviour
data CodeType = Lit | Code | Cfg
  deriving (Show, Eq)

-- Organizing into chunks makes it easier to work with
type Chunk = (CodeType, Text)

litPrefix :: Text
litPrefix = "////"
cfgPrefix :: Text
cfgPrefix = "///!"

-- True if the chunks have the same CodeType
sameCodeType :: Chunk -> Chunk -> Bool
sameCodeType a b = fst a == fst b

-- Separates a Text into various chunks
separateText :: Text -> [Chunk]
               -- Concatenates (with newlines between) the adjacent chunks
               -- (chunks in the same inner array)
               -- [[(Lit, "a"), (Lit, "b")], [(Code, "a")]]
               -- -> [(Lit, "a\nb"), (Code, "a")]
separateText t = fmap flatten
               -- Individual chunks with the same CodeType get grouped together
               -- [Lit, Lit, Code] -> [[Lit, Lit], [Code]]
               -- NE garantees that none of the inner lists are empty
               $ NE.groupBy sameCodeType
               -- Transforms the line into a chunk (decides the type)
               $ fmap toChunk
               -- Split the lines
               $ T.lines t
  where
    toChunk :: Text -> Chunk
    toChunk s
      | litPrefix `T.isPrefixOf` cut = (Lit, T.drop (T.length litPrefix) cut)
      | cfgPrefix `T.isPrefixOf` cut = (Cfg, T.drop (T.length cfgPrefix) cut)
      | otherwise = (Code, s)
      where
        -- For Lit and Cfg, leading and trailing spaces shall be removed
        cut = T.strip s
    flatten :: NonEmpty Chunk -> Chunk
                     -- Gets the type of the first chunk (assuming that all the
                     -- chunks are the same type)
    flatten chunks = let tp = fst $ NE.head chunks
                     -- Joins the Texts of each chunk
                     in  (tp, T.unlines $ fmap snd $ NE.toList chunks)


