{-# LANGUAGE FlexibleContexts #-}
module Text.OrgMode.Elements.Paragraph  where

import           Control.Applicative ((<$>), (<*), (<*>))

import           Data.Functor.Identity

import           Data.Text (Text)
import qualified Data.Text as T

import           Text.Parsec hiding (tokens)
-- import           Text.Parsec.Text

import           Text.OrgMode.Types
import           Text.OrgMode.Parsers.Utils

data Paragraph = TextLine Indent Text | BlankLine Indent deriving Show

instance SectionElement Paragraph

parseParagraph :: Stream Text Identity b => Parsec Text s Paragraph
parseParagraph = try parseBlankLine <|> parseTextLine

parseBlankLine :: Stream Text Identity b => Parsec Text s Paragraph
parseBlankLine = BlankLine <$> indent <* newline

parseTextLine :: Stream Text Identity b => Parsec Text s Paragraph
parseTextLine  = TextLine <$> indent <*> (T.pack <$> manyTill anyChar newline)
