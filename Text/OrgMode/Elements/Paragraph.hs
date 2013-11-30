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

paragraph :: Stream Text Identity b => Parsec Text s Paragraph
paragraph = try blankLine <|> textLine

blankLine :: Stream Text Identity b => Parsec Text s Paragraph
blankLine = BlankLine <$> indent <* newline

textLine :: Stream Text Identity b => Parsec Text s Paragraph
textLine  = TextLine <$> indent <*> (T.pack <$> manyTill anyChar newline)
