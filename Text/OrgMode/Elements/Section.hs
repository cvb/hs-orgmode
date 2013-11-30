{-# LANGUAGE ExistentialQuantification, FlexibleContexts #-}
module Text.OrgMode.Elements.Section  where

import           Control.Applicative ((<$>))

import           Data.Text (Text)

import           Text.Parsec hiding (tokens)

import           Text.OrgMode.Types

import           Text.OrgMode.Elements.Head
import           Text.OrgMode.Elements.Keyword
import           Text.OrgMode.Elements.List
import           Text.OrgMode.Elements.Drawer
import           Text.OrgMode.Elements.Paragraph


data Section = Section [Element] deriving Show

section :: Parsec Text [TodoSeq] Section
section = try tillHead <|> tillEnd
  where
    tillHead = Section <$>
               manyTill element (try $ lookAhead sectionHead)
    tillEnd  = Section <$> many1 element
    -- selem =  SElement <$> parseSElement

element :: Parsec Text [TodoSeq] Element
element =  Element <$> try list
             <|> Element <$> try drawer
             -- <|> Element <$> try parseFootNote
             -- <|> Element <$> try parseTable
             <|> Element <$> paragraph
